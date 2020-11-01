<?php
declare(strict_types=1);

namespace Soong\Task;

use Soong\Contracts\Extractor\Extractor;
use Soong\Contracts\KeyMap\KeyMap;
use Soong\Contracts\Loader\Loader;
use Soong\Contracts\Task\EtlTask as EtlTaskInterface;

/**
 * Implementation of operations for a full ETL process.
 */
class EtlTask extends Task implements EtlTaskInterface
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['extract'] = [
            'required' => true,
            'allowed_types' => 'Soong\Contracts\Extractor\Extractor',
        ];
        $options['transform'] = [
            'allowed_types' => 'Soong\Contracts\Transformer\RecordTransformer[]',
        ];
        $options['load'] = [
            'required' => true,
            'allowed_types' => 'Soong\Contracts\Loader\Loader',
        ];
        $options['key_map'] = [
            'allowed_types' => 'Soong\Contracts\KeyMap\KeyMap',
        ];
        $options['record_factory'] = [
            'required' => true,
            'allowed_types' => 'Soong\Contracts\Data\RecordFactory',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function getExtractor(): ?Extractor
    {
        return $this->getConfigurationValue('extract');
    }

    /**
     * @inheritdoc
     */
    public function getLoader(): ?Loader
    {
        return $this->getConfigurationValue('load');
    }

    /**
     * @inheritdoc
     */
    public function getKeyMap(): ?KeyMap
    {
        return $this->getConfigurationValue('key_map');
    }

    /**
     * Perform an ETL migration operation.
     *
     * @param array $options
     */
    public function migrate(array $options)
    {
        if (empty($this->getExtractor())) {
            // @todo Throw a TaskException.
            return;
        }

        $processedCount = 0;
        $limit = $options['limit'] ?? 0;

        /** @var \Soong\Contracts\Data\RecordFactory $recordFactory */
        $recordFactory = $this->getConfigurationValue('record_factory');

        foreach ($this->getExtractor()->extractFiltered() as $data) {
            $resultData = $recordFactory->create();
            /** @var \Soong\Contracts\Transformer\RecordTransformer $recordTransformer */
            foreach ($this->getConfigurationValue('transform') as $recordTransformer) {
                $resultData = $recordTransformer->transform($data, $resultData);
            }
            $this->getLoader()->load($resultData);
            if (!is_null($this->getKeyMap())) {
                // @todo Handle multi-column keys.
                $extractedKey = $data->getPropertyValue(array_keys($this->getExtractor()->getKeyProperties())[0]);
                $loadedKey = $resultData->getPropertyValue(array_keys($this->getLoader()->getKeyProperties())[0]);
                $this->getKeyMap()->saveKeyMap([$extractedKey], [$loadedKey]);
            }
            $processedCount++;
            if (($limit > 0) && $processedCount >= $limit) {
                return;
            }
        }
    }

    /**
     * Rollback an ETL migration operation.
     *
     * @param array $options
     */
    public function rollback(array $options)
    {
        $processedCount = 0;
        $limit = $options['limit'] ?? 0;
        if (empty($this->getKeyMap())) {
            return;
        }

        foreach ($this->getKeyMap()->iterate() as $extractedKey) {
            $loadedKey = $this->getKeyMap()->lookupLoadedKey($extractedKey);
            if (!empty($loadedKey)) {
                $this->getLoader()->delete($loadedKey);
                $this->getKeyMap()->delete($extractedKey);
                $processedCount++;
                if (($limit > 0) && $processedCount >= $limit) {
                    return;
                }
            }
        }
    }
}
