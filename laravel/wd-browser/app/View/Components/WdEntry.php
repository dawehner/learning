<?php

namespace App\View\Components;

use Illuminate\View\Component;
use Wikidata\SearchResult;

class WdEntry extends Component
{

    private $id;

    /**
     * @var \Wikidata\SearchResult
     */
    private $entry;

    /**
     * @var string
     */
    private $lang;

    /**
     * @var string
     */
    private $label;

    /**
     * @var string
     */
    private $url;

    private string $description;

    /**
     * Create a new component instance.
     *
     * @return void
     */
    public function __construct(
        string $id,
        string $lang,
        string $label,
        string $url,
        string $description = '',
    ) {
        $this->id = $id;
        $this->lang = $lang;
        $this->label = $label;
        $this->url = $url;
        $this->description = $description;
    }

    /**
     * Get the view / contents that represent the component.
     *
     * @return \Illuminate\Contracts\View\View|string
     */
    public function render()
    {
        return view(
            'components.wd-entry',
            [
                'id' => $this->id,
                'lang' => $this->lang,
                'label' => $this->label,
                'url' => $this->url,
                'description' => $this->description,
            ]
        );
    }

}
