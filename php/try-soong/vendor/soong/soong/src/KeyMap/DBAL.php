<?php
declare(strict_types=1);

namespace Soong\KeyMap;

use Doctrine\DBAL\DBALException;
use Doctrine\DBAL\FetchMode;
use Doctrine\DBAL\Schema\Table;
use Soong\Contracts\Exception\KeyMapException;

/**
 * Implementation of key maps using DBAL for storage.
 */
class DBAL extends KeyMapBase
{

    use \Soong\DBAL\DBAL;

    /**
     * Prefix for extractor key's database column names.
     */
    const EXTRACTED_KEY_PREFIX = 'extracted_key_';

    /**
     * Prefix for loader key's database column names.
     */
    const LOADED_KEY_PREFIX = 'loaded_key_';

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
    public function saveKeyMap(array $extractedKey, array $loadedKey) : void
    {
        if (!$this->tableExists()) {
            $this->createTable();
        }
        $extractedKey = array_values($extractedKey);
        $loadedKey = array_values($loadedKey);
        $queryBuilder = $this->connection()->createQueryBuilder();
        $queryBuilder->insert($this->getConfigurationValue('table'));
        $parameterNumber = 0;
        $queryBuilder->setValue('hash', '?');
        $queryBuilder->setParameter($parameterNumber++, $this->hashKeys($extractedKey));
        $counter = 0;
        foreach ($this->extractedKeyColumns() as $columnName) {
            $queryBuilder->setValue($columnName, '?');
            $queryBuilder->setParameter($parameterNumber++, $extractedKey[$counter]);
            $counter++;
        }
        $counter = 0;
        foreach ($this->loadedKeyColumns() as $columnName) {
            if (isset($loadedKey[$counter])) {
                $queryBuilder->setValue($columnName, '?');
                $queryBuilder->setParameter(
                    $parameterNumber++,
                    $loadedKey[$counter]
                );
            }
            $counter++;
        }
        $queryBuilder->execute();
    }

    /**
     * @inheritdoc
     */
    public function lookupLoadedKey(array $extractedKey) : array
    {
        if (!$this->tableExists()) {
            return [];
        }
        $queryBuilder = $this->connection()->createQueryBuilder();
        foreach ($this->loadedKeyColumns() as $keyName => $columnName) {
            $queryBuilder->addSelect("$columnName as $keyName");
        }
        $queryBuilder
          ->from($this->getConfigurationValue('table'))
          ->where('hash = ?')
          ->setParameter(0, $this->hashKeys($extractedKey));
        $result = $queryBuilder->execute()->fetch(FetchMode::ASSOCIATIVE);
        if (!$result) {
            return [];
        }
        return $result;
    }

    /**
     * @inheritdoc
     */
    public function lookupExtractedKeys(array $loadedKey) : array
    {
        if (!$this->tableExists()) {
            return [];
        }
        $queryBuilder = $this->connection()->createQueryBuilder();
        foreach ($this->extractedKeyColumns() as $keyName => $columnName) {
            $queryBuilder->addSelect("$columnName as $keyName");
        }
        $queryBuilder->from($this->getConfigurationValue('table'));
        $loadedKeys = array_values($loadedKey);
        $counter = 0;
        foreach ($this->loadedKeyColumns() as $columnName) {
            $queryBuilder->andWhere("$columnName = ?");
            $queryBuilder->setParameter($counter, $loadedKeys[$counter]);
            $counter++;
        }
        $result = $queryBuilder->execute()->fetchAll(FetchMode::ASSOCIATIVE);
        if (!$result) {
            return [];
        }
        return $result;
    }

    /**
     * @inheritdoc
     */
    public function delete(array $extractedKey) : void
    {
        if (!$this->tableExists()) {
            return;
        }
        $queryBuilder = $this->connection()->createQueryBuilder();
        $queryBuilder->delete($this->getConfigurationValue('table'))
            ->where('hash = ?')
            ->setParameter(0, $this->hashKeys($extractedKey));
        $queryBuilder->execute();
    }

    /**
     * @inheritdoc
     */
    public function count() : int
    {
        if (!$this->tableExists()) {
            return 0;
        }
        try {
            $count = $this->connection()
                ->executeQuery("SELECT COUNT(*) FROM {$this->getConfigurationValue('table')}")
                ->fetchColumn();
        } catch (DBALException $e) {
            print $e->getMessage();
        }
        return (int)$count;
    }

    /**
     * @internal
     *
     * Create the key map table if it doesn't already exist.
     *
     * @throws \Soong\Contracts\Exception\KeyMapException
     */
    protected function createTable()  : void
    {
        if (!$this->tableExists()) {
            try {
                $schema = $this->connection()->getSchemaManager();
            } catch (DBALException $e) {
                throw new KeyMapException('Unable to connect to database.', 0, $e);
            }
            try {
                $map = new Table($this->getConfigurationValue('table'));
                $sampleHash = $this->hashKeys(['foo', 'bar']);
                $map->addColumn(
                    'hash',
                    'string',
                    ['length' => strlen($sampleHash), 'notnull' => true]
                );
                $map->setPrimaryKey(['hash']);
                $counter = 1;
                $indexColumns = [];
                foreach ($this->getConfigurationValue('extractor_keys') as $definition) {
                    $type = $definition['type'];
                    $options = $definition['options'] ?? [];
                    $options['notnull'] = true;
                    $columnName = self::EXTRACTED_KEY_PREFIX . $counter++;
                    $map->addColumn($columnName, $type, $options);
                    $indexColumns[] = $columnName;
                }
                $map->addIndex($indexColumns);
                $counter = 1;
                foreach ($this->getConfigurationValue('loader_keys') as $definition) {
                    $type = $definition['type'];
                    $options = $definition['options'] ?? [];
                    $options['notnull'] = false;
                    $columnName = self::LOADED_KEY_PREFIX . $counter++;
                    $map->addColumn($columnName, $type, $options);
                }
                $schema->createTable($map);
            } catch (DBALException $e) {
                throw new KeyMapException('Unable to create key map table.', 0, $e);
            }
        }
    }

    /**
     * @internal
     *
     * See if the database table exists.
     *
     * @return bool
     *   TRUE if the table exists, FALSE otherwise.
     */
    protected function tableExists() : bool
    {
        try {
            $schema = $this->connection()->getSchemaManager();
        } catch (DBALException $e) {
            throw new KeyMapException('Unable to connect to database.', 0, $e);
        }
        return $schema->tablesExist([$this->getConfigurationValue('table')]);
    }

    /**
     * @internal
     *
     * Construct and return the names of the extracted key columns.
     *
     * @return array
     *   List of extracted key column names, keyed by the extracted key name.
     */
    protected function extractedKeyColumns() : array
    {
        $counter = 1;
        $result = [];
        foreach (array_keys($this->getConfigurationValue('extractor_keys')) as $keyName) {
            $columnName = self::EXTRACTED_KEY_PREFIX . $counter++;
            $result[$keyName] = $columnName;
        }
        return $result;
    }

    /**
     * @internal
     *
     * Construct and return the names of the loaded key columns.
     *
     * @return array
     *   List of loaded key column names, keyed by the loaded key name.
     */
    protected function loadedKeyColumns() : array
    {
        $counter = 1;
        $result = [];
        foreach (array_keys($this->getConfigurationValue('loader_keys')) as $keyName) {
            $columnName = self::LOADED_KEY_PREFIX . $counter++;
            $result[$keyName] = $columnName;
        }
        return $result;
    }

    /**
     * @inheritdoc
     */
    public function iterate() : iterable
    {
        if ($this->tableExists()) {
            $queryBuilder = $this->connection()->createQueryBuilder();
            $queryBuilder->from($this->getConfigurationValue('table'));
            foreach ($this->extractedKeyColumns() as $keyName => $columnName) {
                $queryBuilder->addSelect("$columnName AS $keyName");
            }
            foreach ($queryBuilder->execute() as $row) {
                yield $row;
            }
        }
    }
}
