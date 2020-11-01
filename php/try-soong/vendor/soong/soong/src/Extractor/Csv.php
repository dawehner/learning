<?php
declare(strict_types=1);

namespace Soong\Extractor;

use League\Csv\Reader;
use Soong\Contracts\Data\RecordFactory;

/**
 * CSV extractor based on The League CSV library.
 */
class Csv extends ExtractorBase
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['csv_file_path'] = [
            'required' => true,
            'allowed_types' => 'string',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function extractAll(): iterable
    {
        $csv = $this->loadCsv();
        foreach ($csv->getRecords() as $record) {
            yield $this->getConfigurationValue('record_factory')->create($record);
        }
    }

    /**
     * @inheritdoc
     */
    public function getProperties(): array
    {
        return $this->loadCsv()->getHeader();
    }

    /**
     * @internal
     *
     * Obtain a CSV Reader object for the configured file path.
     *
     * @return Reader
     *   A Reader object representing the CSV file we're extracting.
     */
    protected function loadCsv() : Reader
    {
        $csv = Reader::createFromPath($this->getConfigurationValue('csv_file_path'), 'r');
        // @todo configure the header
        $csv->setHeaderOffset(0);
        return $csv;
    }

    /**
     * @inheritdoc
     */
    public function count()
    {
        $csv = $this->loadCsv();
        return $csv->count();
    }
}
