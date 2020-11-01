<?php
declare(strict_types=1);

namespace Soong\Loader;

use Doctrine\DBAL\DBALException;
use Soong\Contracts\Data\Record;
use Soong\Contracts\Exception\LoaderException;

/**
 * Loader for DBAL SQL tables.
 */
class DBAL extends LoaderBase
{

    use \Soong\DBAL\DBAL;

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['connection'] = [
            'required' => true,
            'allowed_types' => 'array',
        ];
        $options['table'] = [
            'required' => true,
            'allowed_types' => 'string',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function load(Record $data) : void
    {
        try {
            $connection = $this->connection();
        } catch (DBALException $e) {
            throw new LoaderException('Unable to connect to database.', 0, $e);
        }
        try {
            $table = $this->getConfigurationValue('table');
            $connection->insert(
                $table,
                $data->toArray()
            );
            $id = $connection->lastInsertId();
            if ($id) {
                $keyKeys = array_keys($this->getKeyProperties());
                $data->setPropertyValue(reset($keyKeys), $id);
            }
        } catch (DBALException $e) {
            throw new LoaderException("Unable to insert into $table.", 0, $e);
        }
    }

    /**
     * @inheritdoc
     */
    public function delete(array $key) : void
    {
        try {
            $connection = $this->connection();
        } catch (DBALException $e) {
            throw new LoaderException('Unable to connect to database.', 0, $e);
        }
        $queryBuilder = $connection->createQueryBuilder();
        $queryBuilder->delete($this->getConfigurationValue('table'));
        $counter = 0;
        foreach ($key as $name => $value) {
            $queryBuilder->andWhere("$name = ?");
            $queryBuilder->setParameter($counter, $value);
            $counter++;
        }
        $queryBuilder->execute();
        $connection->close();
        $this->connection = null;
    }
}
