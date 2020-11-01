DROP TABLE IF EXISTS `extractsource`;
CREATE TABLE `extractsource` (
  `uniqueid` integer PRIMARY KEY,
  `foo` varchar(255) NOT NULL,
  `bar` varchar(255) NOT NULL,
  `num` int NOT NULL,
  `related` int DEFAULT NULL
);
