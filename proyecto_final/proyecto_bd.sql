
# Consultas BD Proyecto
USE proj_final_draft1;

# Listar todos los pedidos con información del cliente
SELECT oh.order_id, oh.order_date, c.customer_name, oh.shipping_cost
FROM orders_header oh
JOIN customers c ON oh.customer_id = c.customer_id
LIMIT 10;

# Calcular el total de ventas por categoría de producto
SELECT cat.category, SUM(od.sales) as Total_Ventas
FROM order_details od
JOIN products p ON od.product_id = p.product_id
JOIN product_categories sc ON p.subcategory_id = sc.subcategory_id
JOIN categories cat ON sc.category_id = cat.category_id
GROUP BY cat.category
ORDER BY Total_Ventas DESC;

# Encontrar los 5 productos más vendidos
SELECT p.product_name, SUM(od.sales) as cantidad_total
FROM order_details od
JOIN products p ON od.product_id = p.product_id
GROUP BY p.product_id, p.product_name
ORDER BY cantidad_total DESC
LIMIT 5;

# Clientes que han realizado pedidos en más de un mercado (Vacia)
SELECT c.customer_name, COUNT(DISTINCT c.market) as Mercados_Diferentes
FROM customers c
GROUP BY c.customer_id, c.customer_name
HAVING COUNT(DISTINCT c.market) > 1;

# Pedidos con envío prioritario y su costo
SELECT oh.order_id, oh.order_date, sp.order_priority, oh.shipping_cost
FROM orders_header oh
JOIN shipping_priority sp ON oh.priority_id = sp.priority_id
WHERE sp.order_priority = 'Critical'
ORDER BY oh.shipping_cost DESC;

# Calcular el margen de beneficio por producto
SELECT p.product_name, 
       SUM(od.sales) as Ventas_Totales,
       SUM(od.sales * od.discount) as Beneficio_Total,
       (SUM(od.sales * od.discount) / SUM(od.sales)) * 100 as Margen_Beneficio
FROM order_details od
JOIN products p ON od.product_id = p.product_id
GROUP BY p.product_id, p.product_name
HAVING SUM(od.sales) > 0
ORDER BY Margen_Beneficio DESC;

# Pedidos por mes y año
SELECT YEAR(order_date) as Año,
       MONTH(order_date) as Mes,
       COUNT(*) as Total_Pedidos,
       SUM(shipping_cost) as Costo_Envio_Total
FROM orders_header
GROUP BY YEAR(order_date), MONTH(order_date)
ORDER BY Año DESC, Mes DESC;

# Subcategorías con mayor descuento promedio
SELECT sc.subcategory, 
       AVG(od.discount) as Descuento_Promedio,
       COUNT(*) as Total_Productos
FROM order_details od
JOIN products p ON od.product_id = p.product_id
JOIN product_categories sc ON p.subcategory_id = sc.subcategory_id
GROUP BY sc.subcategory_id, sc.subcategory
HAVING COUNT(*) > 10
ORDER BY Descuento_Promedio DESC;

# Tiempo promedio de envío por modo de envío (vacio)
SELECT sm.shipping_mode,
       AVG(DATEDIFF(oh.ship_date, oh.order_date)) as Tiempo_Envio_Promedio
FROM orders_header oh
JOIN shipping_mode sm ON oh.ship_date = sm.shipping_mode_id
WHERE oh.ship_date IS NOT NULL
GROUP BY sm.shipping_mode
ORDER BY Tiempo_Envio_Promedio;

# Clientes con mayor valor total de compras
SELECT c.customer_name, 
       c.segment,
       SUM(od.sales) as Total_Compras,
       COUNT(DISTINCT oh.order_id) as Total_Pedidos
FROM customers c
JOIN orders_header oh ON c.customer_id = oh.customer_id
JOIN order_details od ON oh.order_id = od.order_id
GROUP BY c.customer_id, c.customer_name, c.segment
ORDER BY Total_Compras DESC
LIMIT 10;

# Información completa de ubicación
SELECT c.customer_name, l.city, l.state, l.country, l.region
FROM customers c
JOIN locations l ON c.postal_code = l.postal_code
LIMIT 10;