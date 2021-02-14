<?php

namespace App\View\Components;

use Illuminate\Contracts\Session\Session;
use Illuminate\View\Component;

class BrowserHistoryComponent extends Component
{

    /**
     * @var \Illuminate\Contracts\Session\Session
     */
    private Session $session;

    /**
     * Create a new component instance.
     *
     * @return void
     */
    public function __construct(Session $session)
    {
        $this->session = $session;
    }

    /**
     * Get the view / contents that represent the component.
     *
     * @return \Illuminate\Contracts\View\View|string
     */
    public function render()
    {
        return view('components.browser-history');
    }
}
