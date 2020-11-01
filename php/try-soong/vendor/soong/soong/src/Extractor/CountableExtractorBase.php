<?php
declare(strict_types=1);

namespace Soong\Extractor;

/**
 * Provides default implementation of count().
 */
abstract class CountableExtractorBase extends ExtractorBase implements \Countable
{

    /**
     * @inheritdoc
     */
    public function count()
    {
        // @todo: Implement caching here, and doCount() to get the real count?
        // By default, use brute force.
        $count = 0;
        foreach ($this->extractAll() as $record) {
            $count++;
        }
        return $count;
    }
}
