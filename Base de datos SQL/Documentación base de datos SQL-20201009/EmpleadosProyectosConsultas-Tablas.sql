DROP DATABASE IF EXISTS  EJEMPLO;
CREATE DATABASE EJEMPLO;
USE EJEMPLO;

DROP TABLE IF EXISTS participa;
DROP TABLE IF EXISTS proyecto;

DROP TABLE IF EXISTS empleado;

CREATE TABLE empleado(
            dni char(9) PRIMARY KEY,
            nombre VARCHAR(20),
            apellidos VARCHAR(20),
            salario NUMERIC (6,0)
);

CREATE TABLE proyecto(
            codProyecto char(4) PRIMARY KEY,
            dni char(9),
            descripcion VARCHAR(25),
            anio NUMERIC(4,0),
            FOREIGN KEY (dni) REFERENCES empleado(dni)
);
   
CREATE TABLE participa(
            IdProyecto char(4),
            DNIEmp CHAR(9),
            horas NUMERIC(3,0),
            PRIMARY KEY (IdProyecto, DNIEMP),
            FOREIGN KEY (IdProyecto) REFERENCES proyecto(codProyecto),
            FOREIGN KEY (DNIEmp) REFERENCES empleado(dni)
);
-- Insertar empleados
INSERT INTO empleado VALUES ('78123456J', 'Angela', 'Callejo', 5500);
INSERT INTO empleado VALUES ('78345684K', 'Loreto', 'Borja', 10000);
INSERT INTO empleado VALUES ('78345300I', 'Torcuato', 'Arribas', 15000);
INSERT INTO empleado VALUES ('78334455P', 'Albino', 'Paciencia', 3000);
INSERT INTO empleado VALUES ('78000111H', 'Yolanda', 'Barahona', 18000);
INSERT INTO empleado VALUES ('78434343U', 'Isabel', 'Garcia', 23000);
INSERT INTO empleado VALUES ('78876987P', 'Clara', 'Alamo', 7500);
INSERT INTO empleado VALUES ('78001100J', 'Valentin', 'Riomoros', 7500);
INSERT INTO empleado VALUES ('78321321I', 'Juan Antonio', 'Sanz', 12000);
INSERT INTO empleado VALUES ('78779900T', 'Lucas', 'Gonzalez', 15000);
INSERT INTO empleado VALUES ('78000011V', 'Eustaquia', 'Alonso', 18950);
INSERT INTO empleado VALUES ('78112277J', 'Luis', 'Montero', 3450);
INSERT INTO empleado VALUES ('78123462Z', 'Angel', 'Riocal', 10000);
INSERT INTO empleado VALUES ('78123400M', 'Maribel', 'Calle', 10000);
INSERT INTO empleado VALUES ('78123420J', 'Jose Luis', 'Revoltoso', 10000);

-- Insertar proyecto

INSERT INTO proyecto VALUES ('P001', '78123456J', 'Arduino en la infancia', 2010);
INSERT INTO proyecto VALUES ('P002', '78345684K', 'Bases de datos SQL', 2011);
INSERT INTO proyecto VALUES ('P003', '78345300I', 'C++', 2012);
INSERT INTO proyecto VALUES ('P004', '78345300I', 'Redes neuronales', 2012);
INSERT INTO proyecto VALUES ('P005', '78334455P', 'Apps para moviles', 2011);
INSERT INTO proyecto VALUES ('P006', '78000111H', 'Redes sociales', 2011);
 INSERT INTO proyecto VALUES ('P007', '78000111H', 'Big Data el presente', 2011);
          
-- Insertar participaes
INSERT INTO participa VALUES
      ('P001', '78779900T', 15),
      ('P001', '78123462Z', 8),
       ('P001', '78434343U', 45),
      ('P002', '78321321I', 23),
      ('P002', '78876987P', 3),
       ('P002', '78123462Z', 8),
      ('P002', '78434343U', 45),
      ('P002', '78000011V', 20),
      ('P003', '78321321I', 10),
      ('P003', '78123462Z', 10),
       ('P003', '78434343U', 45),
      ('P004', '78434343U', 10),
      ('P004', '78345300I', 12),
      ('P004', '78876987P', 17),
       ('P004', '78123462Z', 8),
      ('P005', '78876987P', 9),
      ('P005', '78434343U', 8),
       ('P005', '78123462Z', 8),
      ('P006', '78123462Z', 13),
      ('P006', '78434343U', 12),
      ('P006', '78345684K', 8);
     
     
     Select * from empleado;
     select * from proyecto;
     select * from participa;

