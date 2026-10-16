# CASO INTEGRADOR OLIST — TODO EL ANÁLISIS EN UN SOLO BLOQUE DE R
# Autor: Juan Ramos
# Google Colab con entorno R: sube el ZIP desde el panel Archivos y ejecuta.
# RStudio: ejecuta este script; si no encuentra el ZIP, podrás seleccionarlo.
# Alcance: análisis descriptivo; el GMV no representa utilidad ni ingresos de Olist.

# 1. BIBLIOTECAS Y CONFIGURACIÓN ---------------------------------------------
paquetes <- c("dplyr", "tidyr", "readr", "ggplot2", "scales")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) install.packages(faltantes, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))
options(dplyr.summarise.inform = FALSE, width = 150)
theme_set(theme_minimal(base_size = 12))
salida <- "resultados_olist_R"
dir.create(salida, showWarnings = FALSE, recursive = TRUE)
mostrar <- function(x, titulo = NULL) {
  if (!is.null(titulo)) cat("\n", titulo, "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}
grafico <- function(p, nombre, ancho = 11, alto = 5) {
  print(p)
  ggsave(file.path(salida, paste0(nombre, ".png")), p, width = ancho, height = alto, dpi = 150)
}
media <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
pct <- function(x) 100 * media(x)

# 2. ENCONTRAR Y EXTRAER EL ZIP ---------------------------------------------
# Ruta del archivo que aparece en el panel Archivos de Google Colab.
ruta_zip <- "/content/archive (2).zip"
if (!file.exists(ruta_zip)) {
  alternativas <- c(file.path(getwd(), "archive (2).zip"),
    file.path(path.expand("~/Downloads"), "archive (2).zip"),
    file.path(Sys.getenv("USERPROFILE"), "Downloads", "archive (2).zip"))
  alternativas <- alternativas[file.exists(alternativas)]
  if (length(alternativas)) {
    ruta_zip <- alternativas[1]
  } else {
    stop("No existe /content/archive (2).zip. Sube el ZIP o modifica ruta_zip con su ruta exacta.")
  }
}
ruta_zip <- normalizePath(ruta_zip, mustWork = TRUE)
cat("ZIP seleccionado:", ruta_zip, "\n")
archivos <- c(customers = "olist_customers_dataset.csv",
              geolocation = "olist_geolocation_dataset.csv",
              items = "olist_order_items_dataset.csv",
              payments = "olist_order_payments_dataset.csv",
              reviews = "olist_order_reviews_dataset.csv",
              orders = "olist_orders_dataset.csv",
              products = "olist_products_dataset.csv",
              sellers = "olist_sellers_dataset.csv",
              translation = "product_category_name_translation.csv")
indice <- utils::unzip(ruta_zip, list = TRUE)$Name
entradas <- vapply(archivos, function(nombre) {
  coincidencias <- indice[basename(indice) == nombre]
  if (length(coincidencias) != 1L) stop("Archivo ausente o ambiguo dentro del ZIP: ", nombre)
  coincidencias
}, character(1))
carpeta_datos <- tempfile("olist_csv_")
dir.create(carpeta_datos)
utils::unzip(ruta_zip, files = unname(entradas), exdir = carpeta_datos,
             junkpaths = TRUE, overwrite = TRUE)
leer_tabla <- function(nombre) {
  ruta <- file.path(carpeta_datos, nombre)
  if (!file.exists(ruta)) stop("No se pudo extraer: ", nombre)
  # Se leen como texto para conservar identificadores y ceros de prefijos postales.
  read_csv(ruta, col_types = cols(.default = col_character()), show_col_types = FALSE,
           progress = FALSE)
}
tablas <- lapply(archivos, leer_tabla)
numericas <- c("geolocation_lat", "geolocation_lng", "order_item_id", "price", "freight_value",
               "payment_sequential", "payment_installments", "payment_value", "review_score",
               "product_name_lenght", "product_description_lenght", "product_photos_qty",
               "product_weight_g", "product_length_cm", "product_height_cm", "product_width_cm")
for (k in names(tablas)) {
  for (col in intersect(names(tablas[[k]]), numericas))
    tablas[[k]][[col]] <- as.numeric(tablas[[k]][[col]])
}
mostrar(data.frame(tabla = names(tablas), filas = vapply(tablas, nrow, integer(1)),
                   columnas = vapply(tablas, ncol, integer(1))), "Tablas cargadas")

# 3. CALIDAD Y FECHAS -------------------------------------------------------
calidad <- bind_rows(lapply(names(tablas), function(k) data.frame(
  tabla = k, duplicados_exactos = sum(duplicated(tablas[[k]])),
  celdas_nulas_pct = 100 * sum(is.na(tablas[[k]])) / prod(dim(tablas[[k]])))))
mostrar(calidad, "Calidad de los datos")
for (k in c("orders", "reviews", "items")) {
  fechas <- grep("(_timestamp$|_date$)", names(tablas[[k]]), value = TRUE)
  for (col in fechas) tablas[[k]][[col]] <- as.POSIXct(
    tablas[[k]][[col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}
orders <- tablas$orders; customers <- tablas$customers; items <- tablas$items
payments <- tablas$payments; reviews <- distinct(tablas$reviews)
products <- tablas$products; sellers <- tablas$sellers
geo <- tablas$geolocation; translation <- tablas$translation
for (par in list(list(orders, "order_id"), list(customers, "customer_id"),
                 list(products, "product_id"), list(sellers, "seller_id"))) {
  clave <- par[[1]][[par[[2]]]]
  stopifnot(!anyNA(clave), !anyDuplicated(clave))
}
stopifnot(!anyDuplicated(items[c("order_id", "order_item_id")]),
          all(orders$customer_id %in% customers$customer_id),
          all(items$order_id %in% orders$order_id),
          all(items$product_id %in% products$product_id),
          all(items$seller_id %in% sellers$seller_id),
          all(payments$order_id %in% orders$order_id),
          all(reviews$order_id %in% orders$order_id))
mostrar(count(orders, order_status, sort = TRUE), "Estado de los pedidos")
fechas_orders <- grep("timestamp|date", names(orders), value = TRUE)
mostrar(data.frame(campo = fechas_orders,
                   nulos_pct = vapply(orders[fechas_orders], function(x) 100 * mean(is.na(x)), numeric(1))),
        "Fechas ausentes: no se imputan")

# 4. VISTA 360: UNA FILA POR PEDIDO -----------------------------------------
# Primero se agregan artículos, pagos y reseñas para no multiplicar importes.
geo_prefijo <- geo %>% group_by(geolocation_zip_code_prefix) %>%
  summarise(lat = median(geolocation_lat, na.rm = TRUE),
            lon = median(geolocation_lng, na.rm = TRUE), .groups = "drop")
customers <- customers %>% left_join(geo_prefijo %>% rename(
  customer_zip_code_prefix = geolocation_zip_code_prefix,
  customer_lat = lat, customer_lon = lon), by = "customer_zip_code_prefix", relationship = "many-to-one")
sellers <- sellers %>% left_join(geo_prefijo %>% rename(
  seller_zip_code_prefix = geolocation_zip_code_prefix,
  seller_lat = lat, seller_lon = lon), by = "seller_zip_code_prefix", relationship = "many-to-one")
products <- products %>% left_join(translation, by = "product_category_name", relationship = "many-to-one") %>%
  mutate(categoria = coalesce(product_category_name_english, product_category_name, "sin_categoria"),
         volumen_cm3 = product_length_cm * product_height_cm * product_width_cm)
articulos <- items %>% left_join(products, by = "product_id", relationship = "many-to-one") %>%
  left_join(sellers, by = "seller_id", relationship = "many-to-one")
res_items <- articulos %>% group_by(order_id) %>% summarise(
  gmv = sum(price), flete = sum(freight_value), n_articulos = n(),
  n_vendedores = n_distinct(seller_id), n_categorias = n_distinct(categoria), .groups = "drop")
res_pagos <- payments %>% group_by(order_id) %>% summarise(
  pago_total = sum(payment_value), n_registros_pago = n(),
  cuotas_max = max(payment_installments), .groups = "drop")
res_reviews <- reviews %>% group_by(order_id) %>% summarise(
  calificacion = media(review_score), n_resenas = n(),
  respuesta_resena = max(review_answer_timestamp), .groups = "drop")
vista <- orders %>% left_join(customers, by = "customer_id", relationship = "many-to-one") %>%
  left_join(res_items, by = "order_id", relationship = "one-to-one") %>%
  left_join(res_pagos, by = "order_id", relationship = "one-to-one") %>%
  left_join(res_reviews, by = "order_id", relationship = "one-to-one") %>%
  mutate(mes = format(order_purchase_timestamp, "%Y-%m"),
         dias_entrega = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days")),
         dias_atraso = as.numeric(as.Date(order_delivered_customer_date, tz = "UTC") -
                                  as.Date(order_estimated_delivery_date, tz = "UTC")),
         evaluable = order_status == "delivered" & !is.na(dias_atraso) & !is.na(dias_entrega) & dias_entrega >= 0,
         tardio = if_else(evaluable, dias_atraso > 0, NA),
         baja = if_else(!is.na(calificacion), calificacion <= 2, NA),
         flete_pct = if_else(gmv > 0, 100 * flete / gmv, NA_real_))
stopifnot(nrow(vista) == nrow(orders), !anyDuplicated(vista$order_id),
          isTRUE(all.equal(sum(vista$gmv, na.rm = TRUE), sum(items$price))),
          isTRUE(all.equal(sum(vista$flete, na.rm = TRUE), sum(items$freight_value))),
          isTRUE(all.equal(sum(vista$pago_total, na.rm = TRUE), sum(payments$payment_value))))
entregados <- filter(vista, evaluable)
ventas <- filter(vista, order_status == "delivered", !is.na(gmv))
cat("\nVista 360:", nrow(vista), "pedidos. Totales de artículos, flete y pagos conciliados.\n")

# 5. INDICADORES Y EVOLUCIÓN ------------------------------------------------
kpis <- data.frame(indicador = c("Pedidos registrados", "Clientes únicos observados",
  "GMV registrado (R$)", "GMV entregado (R$)", "Flete registrado (R$)",
  "Ticket por pedido con artículos (R$)", "Pedidos con reseña (%)", "Entregados evaluables",
  "Entrega promedio (días)", "Entregas tardías (%)", "Calificación promedio"),
  valor = c(nrow(vista), n_distinct(vista$customer_unique_id), sum(vista$gmv, na.rm = TRUE),
    sum(ventas$gmv), sum(vista$flete, na.rm = TRUE), media(vista$gmv),
    100 * mean(!is.na(vista$calificacion)), nrow(entregados), media(entregados$dias_entrega),
    pct(entregados$tardio), media(vista$calificacion)))
mostrar(kpis, "Indicadores generales")
cat("Período:", format(min(vista$order_purchase_timestamp, na.rm = TRUE)), "a",
    format(max(vista$order_purchase_timestamp, na.rm = TRUE)), "\n")
mensual <- vista %>% group_by(mes) %>% summarise(pedidos = n(), gmv = sum(gmv, na.rm = TRUE), .groups = "drop")
grafico(ggplot(mensual, aes(mes, pedidos, group = 1)) + geom_line(color = "#247c91") +
  geom_point(color = "#247c91") + labs(title = "Pedidos por mes", x = NULL, y = "Pedidos") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)), "01_pedidos_mensuales")
grafico(ggplot(mensual, aes(mes, gmv / 1e6, group = 1)) + geom_line(color = "#247c91") +
  geom_point() + labs(title = "GMV registrado por mes", x = NULL, y = "Millones de R$") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)), "02_gmv_mensual")
# Los meses de los extremos pueden ser incompletos; no atribuir variaciones a crecimiento real sin revisión.

# 6. CLIENTES: RECENCIA, FRECUENCIA Y VALOR ----------------------------------
referencia <- as.Date(max(ventas$order_purchase_timestamp)) + 1
clientes <- ventas %>% group_by(customer_unique_id) %>% summarise(
  ultima_compra = max(order_purchase_timestamp), frecuencia = n_distinct(order_id),
  valor = sum(gmv), calificacion = media(calificacion), .groups = "drop") %>%
  mutate(recencia_dias = as.numeric(referencia - as.Date(ultima_compra)))
umbral <- quantile(clientes$valor, .75, names = FALSE)
clientes <- clientes %>% mutate(segmento = case_when(
  frecuencia >= 2 & recencia_dias <= 90 ~ "Recurrente reciente",
  valor >= umbral & recencia_dias <= 90 ~ "Alto valor reciente",
  valor >= umbral & recencia_dias > 180 ~ "Alto valor inactivo",
  recencia_dias <= 90 ~ "Compra reciente", TRUE ~ "Otros clientes"))
segmentos <- clientes %>% group_by(segmento) %>% summarise(clientes = n(), gmv = sum(valor),
  ticket_cliente = mean(valor), .groups = "drop") %>% arrange(desc(gmv))
mostrar(segmentos, "Segmentos de clientes: umbrales descriptivos")
mostrar(head(arrange(clientes, desc(valor)), 10), "Clientes de mayor gasto observado")
cat(sprintf("Clientes con ≥2 pedidos entregados: %.2f%%\n", 100 * mean(clientes$frecuencia >= 2)))
grafico(ggplot(segmentos, aes(reorder(segmento, gmv), gmv)) + geom_col(fill = "#247c91") +
  coord_flip() + labs(title = "GMV entregado por segmento", x = NULL, y = "R$"), "03_segmentos")
# Recencia y frecuencia dependen de la ventana observada; no son valor futuro del cliente.

# 7. CATEGORÍAS, VENDEDORES Y REGIONES --------------------------------------
detalle <- articulos %>% left_join(vista %>% select(order_id, order_status, customer_state,
  calificacion, baja, tardio, customer_lat, customer_lon, customer_unique_id),
  by = "order_id", relationship = "many-to-one")
detalle_entregado <- filter(detalle, order_status == "delivered")
resumen_grupo <- function(df, claves) {
  dinero <- df %>% group_by(across(all_of(claves))) %>% summarise(
    gmv = sum(price), articulos = n(), .groups = "drop")
  pares <- df %>% distinct(across(all_of(c("order_id", claves))), .keep_all = TRUE)
  experiencia <- pares %>% group_by(across(all_of(claves))) %>% summarise(
    pedidos = n(), resenas = sum(!is.na(calificacion)), nota = media(calificacion),
    bajas_pct = pct(baja), pedidos_resena_baja = sum(baja, na.rm = TRUE),
    entregas_evaluables = sum(!is.na(tardio)), tardios_pct = pct(tardio), .groups = "drop")
  left_join(dinero, experiencia, by = claves, relationship = "one-to-one")
}
categorias <- resumen_grupo(detalle_entregado, "categoria")
vendedores <- resumen_grupo(detalle_entregado, c("seller_id", "seller_state"))
regiones <- ventas %>% group_by(customer_state) %>% summarise(
  pedidos = n(), gmv = sum(gmv), resenas = sum(!is.na(calificacion)), nota = media(calificacion),
  bajas_pct = pct(baja), entregas_evaluables = sum(!is.na(tardio)),
  tardios_pct = pct(tardio), .groups = "drop")
mostrar(head(arrange(categorias, desc(gmv)), 10), "Categorías de mayor GMV entregado")
mostrar(vendedores %>% filter(resenas >= 100) %>% arrange(desc(bajas_pct)) %>% head(10),
        "Vendedores con mayor tasa de reseñas bajas: mínimo 100 reseñas")
mostrar(head(arrange(regiones, desc(gmv)), 10), "Estados del cliente por GMV entregado")
grafico(ggplot(slice_max(categorias, gmv, n = 10, with_ties = FALSE),
  aes(reorder(categoria, gmv), gmv / 1e6)) + geom_col(fill = "#247c91") + coord_flip() +
  labs(title = "Top 10 categorías por GMV entregado", x = NULL, y = "Millones de R$"), "04_categorias")
grafico(ggplot(slice_max(regiones, gmv, n = 10, with_ties = FALSE),
  aes(reorder(customer_state, gmv), gmv / 1e6)) + geom_col(fill = "#247c91") + coord_flip() +
  labs(title = "Top 10 estados por GMV entregado", x = NULL, y = "Millones de R$"), "05_regiones")
# Una reseña es del pedido, no del vendedor o producto individual.
# Un pedido multigrupo aparece en cada grupo; los conteos por grupo no son aditivos. El GMV sí lo es.

# 8. ATRASOS Y SATISFACCIÓN -------------------------------------------------
entregados <- entregados %>% mutate(tipo_entrega = if_else(tardio, "Tardía", "A tiempo"))
logistica <- entregados %>% group_by(tipo_entrega) %>% summarise(pedidos = n(),
  resenas = sum(!is.na(calificacion)), nota = media(calificacion), bajas_pct = pct(baja),
  dias = media(dias_entrega), .groups = "drop")
mostrar(logistica, "Comparación de satisfacción según entrega")
riesgo_t <- logistica$bajas_pct[logistica$tipo_entrega == "Tardía"] / 100
riesgo_a <- logistica$bajas_pct[logistica$tipo_entrega == "A tiempo"] / 100
cat(sprintf("Diferencia: %.1f puntos porcentuales; razón descriptiva: %.2f veces.\n",
            100 * (riesgo_t - riesgo_a), riesgo_t / riesgo_a))
grafico(ggplot(logistica, aes(tipo_entrega, nota, fill = tipo_entrega)) + geom_col() +
  geom_text(aes(label = round(nota, 2)), vjust = -0.4) + ylim(0, 5) +
  labs(title = "Calificación según entrega", x = NULL, y = "Calificación (1–5)") +
  guides(fill = "none"), "06_calificacion_entrega")
grafico(ggplot(logistica, aes(tipo_entrega, bajas_pct, fill = tipo_entrega)) + geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", bajas_pct)), vjust = -0.4) + ylim(0, 100) +
  labs(title = "Reseñas bajas según entrega", x = NULL, y = "% de pedidos con reseña") +
  guides(fill = "none"), "07_bajas_entrega")
entregados <- entregados %>% mutate(tramo_atraso = cut(dias_atraso,
  c(-Inf, 0, 3, 7, 14, Inf), labels = c("A tiempo", "1–3 días", "4–7 días", "8–14 días", "Más de 14 días")))
mostrar(entregados %>% group_by(tramo_atraso) %>% summarise(pedidos = n(),
  resenas = sum(!is.na(calificacion)), nota = media(calificacion), bajas_pct = pct(baja), .groups = "drop"),
  "Satisfacción por días de atraso")

# 9. DISTANCIA Y PESO DEL FLETE --------------------------------------------
haversine <- function(lat1, lon1, lat2, lon2) {
  a <- lat1 * pi / 180; b <- lon1 * pi / 180
  c <- lat2 * pi / 180; d <- lon2 * pi / 180
  h <- sin((c-a)/2)^2 + cos(a)*cos(c)*sin((d-b)/2)^2
  6371 * 2 * asin(sqrt(pmin(1, pmax(0, h))))
}
detalle <- detalle %>% mutate(distancia_km = if_else(
  between(customer_lat, -90, 90) & between(seller_lat, -90, 90) &
    between(customer_lon, -180, 180) & between(seller_lon, -180, 180),
  haversine(customer_lat, customer_lon, seller_lat, seller_lon), NA_real_))
distancias <- detalle %>% group_by(order_id) %>% summarise(
  distancia_km = if (all(is.na(distancia_km))) NA_real_ else max(distancia_km, na.rm = TRUE), .groups = "drop")
entregados <- entregados %>% left_join(distancias, by = "order_id", relationship = "one-to-one") %>%
  mutate(tramo_distancia = cut(distancia_km, c(-Inf, 100, 500, 1000, 2000, Inf),
    labels = c("Hasta 100 km", "100–500 km", "500–1000 km", "1000–2000 km", "Más de 2000 km")),
    tramo_flete = cut(flete_pct, c(-Inf, 10, 20, 40, Inf),
      labels = c("Hasta 10%", "10–20%", "20–40%", "Más de 40%")))
for (variable in c("tramo_distancia", "tramo_flete")) {
  resumen <- entregados %>% group_by(across(all_of(variable))) %>% summarise(pedidos = n(),
    resenas = sum(!is.na(calificacion)), nota = media(calificacion),
    tardios_pct = pct(tardio), bajas_pct = pct(baja), dias = media(dias_entrega), .groups = "drop")
  mostrar(resumen, paste("Logística por", variable))
}
cat(sprintf("Cobertura de distancia: %.1f%%\n", 100 * mean(!is.na(entregados$distancia_km))))
res_dist <- entregados %>% filter(!is.na(tramo_distancia)) %>% group_by(tramo_distancia) %>%
  summarise(tardios_pct = pct(tardio), .groups = "drop")
grafico(ggplot(res_dist, aes(tramo_distancia, tardios_pct)) + geom_col(fill = "#247c91") +
  labs(title = "Atrasos por distancia aproximada", x = NULL, y = "% de entregas tardías") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1)), "08_distancias")
# Distancia en línea recta entre medianas de prefijos; no representa la ruta real.
# En pedidos multivendedor se toma la distancia máxima. No hay causalidad demostrada.

# 10. COMBINACIONES Y PRIORIDADES ------------------------------------------
claves <- c("categoria", "customer_state", "seller_state")
combinaciones <- resumen_grupo(detalle_entregado, claves)
z <- 1.96
prioridades <- combinaciones %>% filter(resenas >= 100) %>% mutate(
  p = pedidos_resena_baja / resenas,
  centro = (p + z^2 / (2 * resenas)) / (1 + z^2 / resenas),
  radio = z * sqrt(p * (1-p) / resenas + z^2 / (4 * resenas^2)) / (1 + z^2 / resenas),
  bajas_ic95_inf_pct = 100 * (centro-radio), bajas_ic95_sup_pct = 100 * (centro+radio)) %>%
  select(-p, -centro, -radio) %>% arrange(desc(pedidos_resena_baja), desc(bajas_pct))
mostrar(head(prioridades, 15), "Combinaciones prioritarias por volumen observado de reseñas bajas")
top_cat <- head(arrange(categorias, desc(gmv))$categoria, 8)
top_est <- head(arrange(regiones, desc(gmv))$customer_state, 7)
mapa <- detalle_entregado %>% distinct(order_id, categoria, customer_state, .keep_all = TRUE) %>%
  group_by(categoria, customer_state) %>% summarise(resenas = sum(!is.na(calificacion)),
    bajas_pct = pct(baja), .groups = "drop") %>% filter(categoria %in% top_cat, customer_state %in% top_est) %>%
  mutate(bajas_pct = if_else(resenas >= 100, bajas_pct, NA_real_),
    categoria = factor(categoria, levels = rev(top_cat)), customer_state = factor(customer_state, levels = top_est))
grafico(ggplot(mapa, aes(customer_state, categoria, fill = bajas_pct)) + geom_tile() +
  geom_text(aes(label = ifelse(is.na(bajas_pct), "", sprintf("%.1f", bajas_pct))), size = 3.5) +
  scale_fill_gradient(low = "#fff2a8", high = "#b30028", na.value = "grey95") +
  labs(title = "Reseñas bajas por categoría y estado del cliente", subtitle = "Solo celdas con ≥100 pedidos con reseña",
    x = "Estado del cliente", y = NULL, fill = "Bajas (%)"), "09_combinaciones", alto = 6)
# Intervalos Wilson descriptivos: no corrigen sesgos de reseñas ni dependencia entre pedidos.

# 11. INDICADOR DE RIESGO CON VALIDACIÓN TEMPORAL ----------------------------
# El atraso real se conoce después de entregar: no se usa como predictor de compra.
# Se restringe a pedidos entregados, con un vendedor y una reseña disponible.
un_vendedor <- articulos %>% semi_join(filter(vista, n_vendedores == 1), by = "order_id") %>%
  distinct(order_id, seller_state)
base_riesgo <- vista %>% filter(order_status == "delivered", n_vendedores == 1,
                               n_resenas == 1, !is.na(baja)) %>%
  left_join(un_vendedor, by = "order_id", relationship = "one-to-one")
corte <- as.POSIXct(quantile(as.numeric(base_riesgo$order_purchase_timestamp), .75, na.rm = TRUE),
                   origin = "1970-01-01", tz = "UTC")
train <- filter(base_riesgo, order_purchase_timestamp < corte, respuesta_resena < corte)
test <- filter(base_riesgo, order_purchase_timestamp >= corte)
global_baja <- media(train$baja)
tasas <- train %>% group_by(customer_state, seller_state) %>% summarise(
  n = n(), malas = sum(baja), .groups = "drop") %>% mutate(
    riesgo_estimado = if_else(n >= 30, (malas + 30 * global_baja) / (n + 30), global_baja))
test <- test %>% left_join(select(tasas, customer_state, seller_state, riesgo_estimado),
  by = c("customer_state", "seller_state"), relationship = "many-to-one") %>%
  mutate(riesgo_estimado = coalesce(riesgo_estimado, global_baja),
    nivel = cut(riesgo_estimado, c(-Inf, global_baja*.8, global_baja*1.2, Inf),
      labels = c("Menor tasa histórica", "Tasa histórica intermedia", "Mayor tasa histórica")))
cat("\nCorte temporal:", format(corte), "| Entrenamiento:", nrow(train), "| Validación:", nrow(test), "\n")
mostrar(test %>% group_by(nivel) %>% summarise(pedidos = n(),
  riesgo_estimado_pct = pct(riesgo_estimado), bajas_observadas_pct = pct(baja), .groups = "drop"),
  "Validación de tasas históricas")
brier <- mean((test$riesgo_estimado - as.numeric(test$baja))^2)
brier_base <- mean((global_baja - as.numeric(test$baja))^2)
cat(sprintf("Brier (menor es mejor): indicador=%.4f; referencia constante=%.4f\n", brier, brier_base))
validacion <- if (brier < brier_base) {
  "Mejora la referencia en este período; requiere validación adicional."
} else {
  "No mejora la referencia constante; no se recomienda adoptarlo sin revisión."
}
cat(validacion, "\nSolo representa pedidos entregados con reseña, no todos los pedidos.\n")

# 12. CONCLUSIONES Y ACCIONES ------------------------------------------------
lider_estado <- slice_max(regiones, gmv, n = 1, with_ties = FALSE)
lider_cat <- slice_max(categorias, gmv, n = 1, with_ties = FALSE)
lider_vendedor <- slice_max(vendedores, gmv, n = 1, with_ties = FALSE)
conclusiones <- c(
  "RESPUESTAS AL CASO INTEGRADOR OLIST",
  sprintf("Mayor GMV entregado por estado: %s, R$ %.2f (%.1f%% del total entregado).",
    lider_estado$customer_state, lider_estado$gmv, 100 * lider_estado$gmv / sum(ventas$gmv)),
  sprintf("Categoría de mayor GMV entregado: %s, R$ %.2f.", lider_cat$categoria, lider_cat$gmv),
  sprintf("Vendedor de mayor GMV entregado: %s (%s), R$ %.2f.",
    lider_vendedor$seller_id, lider_vendedor$seller_state, lider_vendedor$gmv),
  sprintf("Clientes con ≥2 pedidos entregados: %.2f%%.", 100 * mean(clientes$frecuencia >= 2)),
  sprintf("Entregas tardías: %.2f%%; nota tardía %.2f/5 frente a %.2f/5 a tiempo.",
    pct(entregados$tardio), logistica$nota[logistica$tipo_entrega == "Tardía"],
    logistica$nota[logistica$tipo_entrega == "A tiempo"]),
  sprintf("Reseñas bajas: %.1f%% tardías frente a %.1f%% a tiempo; diferencia %.1f puntos porcentuales.",
    100*riesgo_t, 100*riesgo_a, 100*(riesgo_t-riesgo_a)),
  paste("Validación del indicador de riesgo:", validacion))
if (nrow(prioridades)) {
  p <- prioridades[1, ]
  conclusiones <- c(conclusiones, sprintf(
    "Prioridad por volumen de reseñas bajas: categoría %s, cliente %s, vendedor %s: %d bajas de %d reseñas (%.1f%%; IC95%% %.1f–%.1f%%).",
    p$categoria, p$customer_state, p$seller_state, p$pedidos_resena_baja, p$resenas,
    p$bajas_pct, p$bajas_ic95_inf_pct, p$bajas_ic95_sup_pct))
}
cat("\n", paste(conclusiones, collapse = "\n"), "\n")
acciones <- data.frame(
  prioridad = c("Reducir atrasos", "Gestionar vendedores", "Proteger ventas", "Retener clientes", "Vigilar señales tempranas"),
  accion = c("Auditar preparación y rutas de grupos prioritarios; revisar promesas de entrega.",
    "Revisar vendedores con ≥100 reseñas y tasas bajas elevadas; separar pedidos multivendedor.",
    "Asegurar capacidad en estados y categorías de mayor GMV entregado.",
    "Probar campañas de recompra por segmento con grupo de control.",
    "Usar tasas históricas solo si superan la referencia y mantienen calibración."),
  seguimiento = c("Tasa y días de atraso; reseñas bajas antes/después.",
    "Reseñas bajas, volumen y cumplimiento por vendedor.", "GMV entregado, cancelaciones y cumplimiento.",
    "Recompra y GMV incremental frente al control.", "Brier, cobertura y resultados posteriores."))
mostrar(acciones, "Acciones propuestas")
limites <- c("LIMITACIONES",
  "Datos históricos: no describen automáticamente el mercado actual.",
  "GMV, pagos e ingresos de Olist son conceptos distintos; faltan costos para medir utilidad.",
  "Las reseñas ausentes pueden sesgar resultados; no equivalen a satisfacción.",
  "La reseña pertenece al pedido: no prueba responsabilidad de un vendedor particular.",
  "Distancias aproximadas; no hay rutas reales ni identificación del transportista.",
  "Las asociaciones no demuestran causalidad ni controlan todas las diferencias entre grupos.",
  "Las tasas históricas se validan solo entre pedidos entregados con reseña.",
  "Los umbrales de volumen reducen ruido sin garantizar representatividad.")
cat("\n", paste(limites, collapse = "\n"), "\n")

# 13. EXPORTACIONES --------------------------------------------------------
exportaciones <- list(vista_360_pedidos = vista, segmentacion_clientes = clientes,
  resumen_categorias = categorias, resumen_vendedores = vendedores, resumen_regiones = regiones,
  combinaciones_prioritarias = prioridades, logistica_satisfaccion = logistica,
  calidad_datos = calidad, indicadores = kpis, acciones_recomendadas = acciones)
for (nombre in names(exportaciones)) write_csv(exportaciones[[nombre]],
  file.path(salida, paste0(nombre, ".csv")), na = "")
writeLines(c(conclusiones, "", limites), file.path(salida, "conclusiones.txt"), useBytes = TRUE)
cat("\nAnálisis terminado. Resultados y gráficos en:", normalizePath(salida), "\n")
