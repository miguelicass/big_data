## Create
CREATE TABLE company ( 
	id_company INT AUTO_INCREMENT,
	name VARCHAR(50) UNIQUE NOT NULL, 
	country VARCHAR(25),
    email VARCHAR(50) UNIQUE ,
	fun_date DATE NOT NULL,
    website VARCHAR(100),
PRIMARY KEY(id_company));


CREATE TABLE worker ( 
	dni CHAR(9), -- Spain identifier
	name VARCHAR(50) NOT NULL, 
	country VARCHAR(25),
    email VARCHAR(50) UNIQUE,
	post_code INT,
    street VARCHAR(50),
    num SMALLINT,
PRIMARY KEY(dni));


CREATE TABLE job ( 
	id_company INT,
	dni_worker CHAR(9),
	init_date DATE NOT NULL,
	end_date DATE, -- NULL => still working
PRIMARY KEY (id_company, dni_worker),
FOREIGN KEY (id_company) REFERENCES company(id_company)
	ON DELETE CASCADE,
	-- ON UPDATE CASCADE, -- value auto incremented, not real use for ON UPDATE CASCADE.
    -- este como otros update en cascada podria tener sentido si cambiaramos a otro identificador
    -- por el rango de int en cuyo rango caben 2147483647 (4294967295 sin signo) empresas
    -- este criterio se aplicara para todos los demas ON UPDATE CASCADE con ID
FOREIGN KEY (dni_worker) REFERENCES worker(dni) 
	ON DELETE CASCADE);
	-- ON UPDATE CASCADE);  -- the same dni until delete, not real use for ON UPDATE CASCADE
    -- podria tener sentido si quisieramos actualizar a un valor diferente que englabara mas
	-- documentos de identificacion de otros paises


CREATE TABLE app ( 
	id_app INT AUTO_INCREMENT,
	name VARCHAR(50) NOT NULL UNIQUE, 
    price NUMERIC(5,2), -- max price 999.00$
	storage FLOAT,
    init_date DATE NOT NULL,
	end_date DATE, -- NULL => because it could be a alpha/beta app 
    id_company INT NOT NULL,
	dni_manager CHAR(9) NOT NULL,
    PRIMARY KEY(id_app),
    FOREIGN KEY (id_company) REFERENCES company(id_company),
		-- ON DELETE CASCADE -- if a company disappeare, not theirs app
		-- ON UPDATE CASCADE,  -- value auto incremented, not real use for ON UPDATE CASCADE
    FOREIGN KEY (dni_manager) REFERENCES worker(dni));
		-- ON DELETE CASCADE -- if a worker death, not delete the app which managed
		-- ON UPDATE CASCADE);  -- value auto incremented, not real use for ON UPDATE CASCADE


CREATE TABLE develop ( 
	dni_worker CHAR(9),
	id_app INT,
	PRIMARY KEY (dni_worker, id_app),
	FOREIGN KEY (dni_worker) REFERENCES worker(dni)
		ON DELETE CASCADE,
		-- ON UPDATE CASCADE);  -- value auto incremented, not real use for ON UPDATE CASCADE
    FOREIGN KEY (id_app) REFERENCES app(id_app)
	ON DELETE CASCADE);
	-- ON UPDATE CASCADE, -- the same dni until delete, not real use for ON UPDATE CASCADE


CREATE TABLE user ( 
	id_user INT AUTO_INCREMENT,
	name VARCHAR(50) NOT NULL, 
    phone  NUMERIC(9,0) UNIQUE, -- Spanish number telephone 9 digits
	country VARCHAR(25),
    current_account CHAR(24) UNIQUE, -- Spain 24 digits,  ES80 2310 0001 1800 0001 2345 (without spaces) 
	post_code INT,
    street VARCHAR(50),
    num SMALLINT,
PRIMARY KEY(id_user));


CREATE TABLE download ( 
	id_user INT,
	id_app INT,
    points SMALLINT,  -- From 0 to 10
	comentary varchar(250),
	down_date DATE NOT NULL,
PRIMARY KEY(id_user, id_app),
FOREIGN KEY (id_user) REFERENCES user(id_user)
	ON DELETE CASCADE, 
	-- ON UPDATE CASCADE,  -- value auto incremented, not real use for ON UPDATE CASCADE
FOREIGN KEY (id_app) REFERENCES app(id_app)
	ON DELETE CASCADE,
	-- ON UPDATE CASCADE,  -- value auto incremented, not real use for ON UPDATE CASCADE
CHECK ((points >= 0 AND points <= 10) OR points = NULL)); -- From 0.00 to 10.00, uless the user not pointed, points = NULL


CREATE TABLE category ( 
	id_category INT AUTO_INCREMENT,
    name VARCHAR(50) UNIQUE,
PRIMARY KEY(id_category));


CREATE TABLE app_category ( 
	id_category INT,
	id_app INT,
PRIMARY KEY(id_category, id_app),
FOREIGN KEY (id_category) REFERENCES category(id_category)
	ON DELETE CASCADE,
	-- ON UPDATE CASCADE,  -- value auto incremented, not real use for ON UPDATE CASCADE
FOREIGN KEY (id_app) REFERENCES app(id_app)
	ON DELETE CASCADE);
	-- ON UPDATE CASCADE);  -- value auto incremented, not real use for ON UPDATE CASCADE


CREATE TABLE store ( 
	id_store INT AUTO_INCREMENT,
	name VARCHAR(50) NOT NULL UNIQUE, 
    website VARCHAR(100),
	-- name_company VARCHAR(50) NOT NULL, -- este campo ha sido sustituido por mi verdadera clave foranea => id_company
	id_company INT NOT NULL,
PRIMARY KEY(id_store),
FOREIGN KEY (id_company) REFERENCES company(id_company)
	ON DELETE CASCADE); -- if the company disappeare, the store too. 
	-- ON UPDATE CASCADE);  -- value auto incremented, not real use for ON UPDATE CASCADE


CREATE TABLE app_store ( 
	id_app INT,
	id_store INT,
PRIMARY KEY(id_app, id_store),
FOREIGN KEY (id_store) REFERENCES store(id_store)
	ON DELETE CASCADE,
	-- ON UPDATE CASCADE);  -- value auto incremented, not real use for ON UPDATE CASCADE
FOREIGN KEY (id_app) REFERENCES app(id_app)
	ON DELETE CASCADE);
	-- ON UPDATE CASCADE,  -- value auto incremented, not real use for ON UPDATE CASCADE

-- DROP TABLE IF EXISTS company;


## Insert
-- insert the companies who has a marketplace
INSERT INTO company (name, country, email, fun_date, website)
VALUES
	('Apple', 'United State', 'apple@icloud.com', '1998-01-011', 'http://apple.com/'),
	('Google Android', 'United State', 'google@gmail.com', '1976-01-01', 'http://android.com/'),
    ('Black Berry', 'Canada', 'blackberry@mail.com', '1984-01-01', 'http://blackberry.com/'),
	('WindowsPhone', 'United State', 'windowsphone@mail.com', '2010-01-011', 'http://windowsphone.com/'),
	('Nokia', 'Finland', 'nokia@mail.com', '1865-01-01', 'http://nokia.com/'),
    ('HP', 'United State', 'hp@mail.com', '1939-01-01', 'http://hp.com/'),   
    ('Amazon', 'United State', 'amazon@mail.com', '1994-01-01', 'http://amazon.com/');
  
  
  -- insert the main stores
INSERT INTO store (name, website, id_company)
VALUES 
	('App Store', 'https://www.apple.com/es/ios/app-store/', 1),
	('Gogle Play Store', 'https://play.google.com/store/', 2),
    ('App World', 'http://appworld.com/', 3),
	('Market Place', 'http://apple.com/', 4),
	('OVITienda', 'http://ovitienda.com/', 5),
    ('AppCatalog', 'http://appcatalog.com/', 6),
    ('Appstore', 'http://appstore.amazon.com/', 7);
  
  
-- insert all categories (10)
INSERT INTO category (name)
VALUES
	('Tools'),('Communication'),('Bussines'),('Social'),('Travel & Local'),
    ('Music & Audio'),('Entertainment'),('Photography'),('Lifestyle'),('Shopping'),
    ('Education'),('Finance'),('Food & Drink'),('Health & Fitness'),('House & Home'),
    ('Maps & Navigation'),('Sports'),('Weather'),('Art & Design'),('Productivity'),
	('Dating'),('Events'),('Beauty'),('Auto & Vehicles	'),('Games');


## Queries
-- 1) Tiempo de desarrollo medio en una app
SELECT avg( 
	DATEDIFF( 
		IF(end_date IS NOT NULL, end_date, 
			NOW()), 
            init_date 
		) 
    ) as avg_develop_days 
FROM app;

-- 2) Usuarios que han comentado en alguna aplicacion con la palabra good
SELECT u.id_user, u.name, d.comentary
FROM download as d
LEFT JOIN user as u ON u.id_user = d.id_user 
WHERE d.comentary like '%good%';

-- 3) Aplicacion que mas espacio ocupa
SELECT * -- id_app, name, storage 
FROM app
where storage =
	(SELECT max(storage) 
	FROM app);
    
-- 4) numero de empleados activos 
-- 4.A) subconsulta
SELECT count(distinct(w.dni)) as num_workers
FROM worker as w
WHERE w.dni IN 
    (SELECT j.dni_worker 
	FROM job as j
	WHERE j.end_date IS NULL );
-- 4.B) JOIN
SELECT count(distinct(w.dni)) as num_workers 
FROM job as j
LEFT JOIN worker as w ON w.dni = j.dni_worker
LEFT JOIN company as c ON  c.id_company = j.id_company
WHERE j.end_date IS NULL;

-- 5) Cual es el numero de desarrolladores medio por app
SELECT avg(aux.num_dev) as avg_developers
FROM(
	SELECT d.id_app, a.name, count(d.id_app) as num_dev
	FROM develop as d
	LEFT JOIN worker as w ON w.dni = d.dni_worker
	LEFT JOIN app as a ON a.id_app = d.id_app
	GROUP BY  d.id_app, a.name) as aux;
    
-- 6) Empleados jubilados o que ya no trabajan en un determinado puesto 
SELECT w.dni, w.name, w.email, c.name , j.init_date, j.end_date
FROM job as j
LEFT JOIN worker as w ON w.dni = j.dni_worker
LEFT JOIN company as c ON  c.id_company = j.id_company
WHERE j.end_date IS NOT NULL;

-- 7) Empleado que mas tiempo lleva en una empresa
SELECT w.dni, w.name, w.email, c.name as company, 
	DATEDIFF(IF(j.end_date IS NOT NULL, j.end_date, NOW()), j.init_date) as work_days
FROM job as j
LEFT JOIN worker as w ON w.dni = j.dni_worker
LEFT JOIN company as c ON  c.id_company = j.id_company
ORDER BY work_days DESC
LIMIT 1;

-- 8) Compañia donde mas empleado ha trabajado en toda su historia
SELECT c.id_company, c.name, count(c.id_company) as num_work
FROM job as j
LEFT JOIN worker as w ON w.dni = j.dni_worker
LEFT JOIN company as c ON  c.id_company = j.id_company
GROUP BY c.id_company, c.name
ORDER BY count(c.id_company) DESC
LIMIT 1;

-- 9) Empresas con 5 o mas trabajadores en activo
SELECT c.id_company, c.name as company, count(c.id_company) as num_workers
FROM job as j
LEFT JOIN company as c ON c.id_company = j.id_company
LEFT JOIN worker as w ON w.dni = j.dni_worker
WHERE j.end_date IS NOT NULL
GROUP BY c.id_company, c.name
HAVING count(c.id_company) > 4;

-- 10) Cual es la aplicacion con menos descargas, y a que empresa pertenece
SELECT d.id_app, a.name as app, count(d.id_app) as num_down
FROM download as d
LEFT JOIN user as u ON u.id_user = d.id_user
LEFT JOIN app as a ON a.id_app = d.id_app
GROUP BY  d.id_app, a.name
ORDER BY num_down ASC
LIMIT 1;


-- 11) Managers que dirigen mas de una aplicacion
SELECT w.dni, w.name, a.num_apps as 'num apps' 
FROM 
	(SELECT dni_manager, count(dni_manager) as num_apps
	FROM app
	GROUP BY dni_manager
	HAVING count(dni_manager) > 1) as a
LEFT JOIN worker as w ON w.dni = a.dni_manager;

-- 12) aplicacion más puntuada
SELECT a.id_app, a.name, p.points 
FROM 
	(SELECT id_app, avg(points) as points
	FROM download
	GROUP BY id_app) as p
LEFT JOIN app as a ON p.id_app = a.id_app
ORDER BY p.points DESC
LIMIT 1;

-- 13) El usuario que mas a pagado por compra de apps
SELECT *
FROM (
	SELECT d.id_user, u.name, count(d.id_user), sum(a.price) as price
	FROM download as d
	LEFT JOIN user as u ON u.id_user = d.id_user
	LEFT JOIN app as a ON a.id_app = d.id_app
	GROUP BY  d.id_user, name) as aux
ORDER BY aux.price DESC
LIMIT 1;

-- 14) Las 5 stores con mas aplicaciones y su respectivas empresas
SELECT c.name as comapny, aux.name, aux.num_apps
FROM
	(SELECT s.id_store, s.name, s.id_company, count(s.id_store) as num_apps
	FROM app_store as ass
	LEFT JOIN app as a ON a.id_app = ass.id_app
	LEFT JOIN store as s ON s.id_store = ass.id_store
	GROUP BY s.id_store, s.name, s.id_company) aux
LEFT JOIN company as c ON c.id_company = aux.id_company
ORDER BY aux.aux.num_apps DESC
LIMIT 5;

-- 15) Aplicacion con mas tags de categorias y su respectivas categorias
SELECT aa.name, cc.name
FROM app as aa 
LEFT JOIN app_category as aacc ON aa.id_app = aacc.id_app
LEFT JOIN category as cc ON  cc.id_category = aacc.id_category
WHERE aa.id_app = 
	(SELECT aux.id_app
	FROM
		(SELECT ac.id_app, count(ac.id_app) as num_category
		FROM app_category as ac
		LEFT JOIN app as a ON a.id_app = ac.id_app
		LEFT JOIN category as c ON  c.id_category = ac.id_category
		GROUP BY ac.id_app) aux
	ORDER BY aux.num_category DESC
	LIMIT 1);


