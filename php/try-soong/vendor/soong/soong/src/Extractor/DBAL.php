<?php
declare(strict_types=1);

namespace Soong\Extractor;

use Doctrine\DBAL\DBALException;
use Doctrine\DBAL\FetchMode;
use Soong\Contracts\Exception\ExtractorException;

/**
 * Extractor for DBAL SQL queries.
 */
class DBAL extends CountableExtractorBase
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
        $options['query'] = [
            'required' => true,
            'allowed_types' => 'string',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function extractAll() : iterable
    {
        try {
            $connection = $this->connection();
        } catch (DBALException $e) {
            throw new ExtractorException('Unable to connect to database.', 0, $e);
        }
        try {
            // @todo: don't accept raw SQL from configuration
            /** @var \Doctrine\DBAL\Driver\Statement $statement */
            $query = $this->getConfigurationValue('query');
            $statement = $connection->executeQuery($query);
        } catch (DBALException $e) {
            throw new ExtractorException("Error executing query '$query'.", 0, $e);
        }
        while ($row = $statement->fetch(FetchMode::ASSOCIATIVE)) {
            yield $this->getConfigurationValue('record_factory')->create($row);
        }
        $this->connection->close();
        unset($this->connection);
    }

    /**
     * @inheritdoc
     */
    public function getProperties(): array
    {
        // @todo: Identify properties from the query.
        return parent::getProperties();
    }

    // @todo implement count() to use SQL COUNT().
}
