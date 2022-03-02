DROP DATABASE IF EXISTS tiendaInformatica;

CREATE DATABASE TiendaInformatica;
USE tiendaInformatica;
DROP TABLE IF EXISTS compra;
DROP TABLE IF EXISTS cliente;
DROP TABLE IF EXISTS articulo;

CREATE TABLE cliente (
   CodCliente  char(3),
   nombreC varchar(40) not null,
   direccion varchar(40) not null,
   telefono  numeric(9,0),
   PRIMARY KEY(CodCliente)
);

CREATE TABLE articulo (
   codArticulo  char(4),
   denom    varchar(40) not null,
   precio    numeric(6,2) not null,
   unidades    integer,
   descuento numeric(5,2) DEFAULT 0,
   precioFinal numeric(6,2) , -- campo calculado
   PRIMARY KEY(codArticulo)
);

CREATE TABLE compra (
   idCliente     char(3),
   idArticulo     char(4),
   fecCompra  date  NOT NULL,
   numUnidades   integer,
   PRIMARY KEY(idCliente, idArticulo,fecCompra),
   FOREIGN KEY(idCliente) REFERENCES cliente(CodCliente) ON DELETE cascade,   
   FOREIGN KEY(idArticulo) REFERENCES articulo(codArticulo) ON DELETE cascade
);

ALTER TABLE compra 
ADD CONSTRAINT numUnidades CHECK (numUnidades >0);-- así si funciona


INSERT INTO cliente VALUES ('008', 'Torcuato Montero', 'Rio  Duero 14', 937846308);
INSERT INTO cliente VALUES ('009', 'Asuncion Rodríguez', 'Pez 14', 914565308);
INSERT INTO cliente VALUES ('010', 'Eustquia Alonso', 'Rio Lozoya 35', 917845208);
INSERT INTO cliente VALUES ('011', 'Angela Callejo',  'Pedro Villar 330',  914849303);
INSERT INTO cliente VALUES ('012', 'Maribel Riocal',  'Luna 11', 914394943);
INSERT INTO cliente VALUES ('013', 'Juan Antonio Sanz', 'Clavel 21',      915656501);
INSERT INTO cliente VALUES ('014', 'Clara Garcia', 'Cercona 57', 913389307);
INSERT INTO cliente VALUES ('015', 'Isabel Sanrio', 'Travesia del rio 14', 917845308);
INSERT INTO cliente VALUES ('016', 'Eugenio Arribas', 'Tinajas 14', 917845308);

INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0001', 'Ordenador Sobremesa',     600, 12);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0002', 'Ordenador Portátil',     1000,  6);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0003', 'Tarjeta Red',         20, 25);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0004', 'Impresora Láser',    200,  4);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0005', 'Ratón USB',            7, 50);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0006', 'Monitor TFT',        250, 10);
INSERT INTO articulo(codArticulo, denom,precio,unidades) VALUES ('0007', 'Router inalámbrico', 100, 30);



INSERT INTO compra VALUES('011', '0001', '25/10/06', 1);
INSERT INTO compra VALUES('011', '0005', '26/10/06', 2);
INSERT INTO compra VALUES('012', '0002', '01/11/06', 1);
INSERT INTO compra VALUES('012', '0003', '01/11/06', 3);
INSERT INTO compra VALUES('013', '0006', '27/10/06', 2);
INSERT INTO compra VALUES('013', '0003', '27/10/06', 2);
INSERT INTO compra VALUES('015', '0004', '03/11/06', 1);
INSERT INTO compra VALUES('015', '0002', '24/11/06', 1);
INSERT INTO compra VALUES('015', '0007', '15/11/06', 45);


-- TIMESTAMPDIFF(YEAR,FecNac,CURDATE())  calcular la edad de una persona
-- now() momento actual
select now();
-- now()-interval 5 year momento actual menos 5 años
select now()-interval 5 year;
-- month(curdate()) mes de la fecha actual
select month(curdate());
-- case when
select (CASE  WHEN (precio>100)   THEN  precio * 1.05				 
  ELSE precio * 1.00  END) as precioIncrementado
  from articulo;
  select  * from articulo; 


-- 	Haz una consulta de la tabla artículo para comprobar como ha rellenado el nuevo atributo.
-- Listado de clientes en orden inverso al alfabético.
-- Código de los clientes que han realizado compras, no se debe repetir el código del cliente si éste ha comprado 2 veces.
-- Nombre y precio de cada uno de los artículos si les incrementamos el precio el 10%.
-- En nuestra tienda es muy importante llamar a las personas por su nombre, me gustaría ver la dirección que tienen todos los que se llaman Pepe. 
-- Nombre de los artículos de los que me quedan entre 3 y 10 unidades.
-- Nombre de los artículos ordenados descendentemente por el número de unidades que me quedan en el almacén.
-- Código de los artículos adquiridos por el cliente 015 
-- Código del cliente que ha comprado una unidad entre el 2000 y el 2005
-- 	Código de los clientes que ha comprado algún ratón
-- 	Código de los clientes que ha comprado algún artículo este año
-- 	Precio medio de los artículos del almacén
-- Nombre del cliente y nombre de los artículos que ha adquirido
-- 	Insertar los datos de un artículo Ipad que cuesta 1000 euros, tenemos 10 unidades y tiene un descuento del 10%
-- Insertar los datos necesarios para que los clientes 014 y 015 compren dos Ipad cada uno con fecha de hoy
-- Nombre y teléfono de los clientes que han comprado un IPad
-- 	Listado del cliente y el número total de artículos adquiridos por éste 
-- 	Nombre de los clientes que ha comprado más de 2 tarjetas de red
-- 	Calcular el número medio de unidades vendidas de los artículos
-- Listado con el número de unidades compradas de cada artículo
-- 	Obtener el número medio de unidades vendidas por cliente
-- 	Código del cliente que más unidades ha adquirido de algún artículo 
-- Nombre del cliente que más unidades ha adquirido de algún artículo 
-- Clientes que han comprado más de 2 artículos
-- Nombre de los clientes que han adquirido más artículos que la media
-- Eliminar el cliente con código ‘011’ y ver lo que ha ocurrido con sus compras
-- 	Obtener el precio medio de venta de los artículos de la empresa
