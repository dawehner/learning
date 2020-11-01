<?php
declare(strict_types=1);

namespace Soong\Extractor;

/**
 * Extractor for in-memory arrays.
 */
class ArrayExtractor extends CountableExtractorBase
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['data'] = [
            'required' => true,
            'allowed_types' => 'iterable',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function extractAll() : iterable
    {
        /** @var \Soong\Contracts\Data\RecordFactory $recordFactory */
        $recordFactory = $this->getConfigurationValue('record_factory');
        foreach ($this->getConfigurationValue('data') as $data) {
            yield $recordFactory->create($data);
        }
    }

    /**
     * @inheritdoc
     */
    public function getProperties(): array
    {
        return array_keys($this->getConfigurationValue('data')[0]);
    }

    /**
     * @inheritdoc
     */
    public function count()
    {
        return count($this->getConfigurationValue('data'));
    }
}
