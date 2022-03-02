 
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
-- SELECT * FROM company
