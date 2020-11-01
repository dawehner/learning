<?php
declare(strict_types=1);

namespace Soong\DBAL;

use Doctrine\DBAL\Connection;
use Doctrine\DBAL\DriverManager;

/**
 * Common implementation for Soong's DBAL implementations.
 */
trait DBAL
{

    /**
     * @internal
     *
     * The DBAL Connection.
     *
     * @var Connection
     */
    protected $connection;

    /**
     * Make (if necessary) and return the connection to the specified database.
     *
     * @return Connection
     *
     * @throws \Doctrine\DBAL\DBALException
     */
    protected function connection() : Connection
    {
        if (empty($this->connection)) {
            $this->connection = DriverManager::getConnection($this->getConfigurationValue('connection'));
        }
        return $this->connection;
    }

    abstract public function getConfigurationValue(string $optionName);
}
