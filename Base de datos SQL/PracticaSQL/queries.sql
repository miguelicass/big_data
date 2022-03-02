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


