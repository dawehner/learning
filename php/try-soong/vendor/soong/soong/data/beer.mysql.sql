DROP TABLE IF EXISTS `beer`;
CREATE TABLE `beer` (
  `id` int NOT NULL AUTO_INCREMENT,
  `name` varchar(255) DEFAULT NULL,
  `body` text DEFAULT NULL,
  `excerpt` varchar(255) DEFAULT NULL,
  `userid` int DEFAULT NULL,
  `image_path` varchar(255) DEFAULT NULL,
  `image_alt` varchar(255) DEFAULT NULL,
  `image_title` varchar(255) DEFAULT NULL,
  `image_description` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
);
DROP TABLE IF EXISTS `beer_terms`;
CREATE TABLE `beer_terms` (
  `id` int NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `parent` varchar(255) DEFAULT NULL,
  `region` varchar(255) DEFAULT NULL,
  `hoppiness` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
);
DROP TABLE IF EXISTS `beer_users`;
CREATE TABLE `beer_users` (
  `id` int NOT NULL AUTO_INCREMENT,
  `published` int DEFAULT NULL,
  `registration_date` varchar(20) DEFAULT NULL,
  `username` varchar(255) DEFAULT NULL,
  `display_name` varchar(255) DEFAULT NULL,
  `pass` varchar(255) DEFAULT NULL,
  `email` varchar(255) NOT NULL,
  `taster` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`)
);
