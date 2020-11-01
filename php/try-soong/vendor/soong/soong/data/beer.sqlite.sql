DROP TABLE IF EXISTS `beer`;
CREATE TABLE `beer` (
  `id` integer PRIMARY KEY,
  `name` varchar(255) DEFAULT NULL,
  `body` text DEFAULT NULL,
  `excerpt` varchar(255) DEFAULT NULL,
  `userid` int DEFAULT NULL,
  `image_path` varchar(255) DEFAULT NULL,
  `image_alt` varchar(255) DEFAULT NULL,
  `image_title` varchar(255) DEFAULT NULL,
  `image_description` varchar(255) DEFAULT NULL
);
DROP TABLE IF EXISTS `beer_terms`;
CREATE TABLE `beer_terms` (
  `id` integer PRIMARY KEY,
  `name` varchar(255) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `parent` varchar(255) DEFAULT NULL,
  `region` varchar(255) DEFAULT NULL,
  `hoppiness` varchar(255) DEFAULT NULL,
  UNIQUE(`name`)
);
DROP TABLE IF EXISTS `beer_users`;
CREATE TABLE `beer_users` (
  `id` integer PRIMARY KEY,
  `published` int DEFAULT NULL,
  `registration_date` varchar(20) DEFAULT NULL,
  `username` varchar(255) DEFAULT NULL,
  `display_name` varchar(255) DEFAULT NULL,
  `pass` varchar(255) DEFAULT NULL,
  `email` varchar(255) NOT NULL,
  `taster` varchar(255) DEFAULT NULL
);
