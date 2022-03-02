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