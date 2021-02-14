<?php

namespace Tests\Feature;

use Illuminate\Foundation\Testing\RefreshDatabase;
use Illuminate\Foundation\Testing\WithFaker;
use Tests\TestCase;

class BrowserHistoryListenerTest extends TestCase
{

    /**
     * A basic feature test example.
     *
     * @return void
     */
    public function test_example()
    {
        $this->startSession();
        $response = $this->withSession([])->get('/');
        $this->assertEquals(['/'], $this->app['session']->get('history'));

        $response = $this->withSession(['history' => ['/']])->get('/welcome');
        $this->assertEquals(['/', '/welcome'], $this->app['session']->get('history'));

        $response = $this->withSession(['history' => ['/test']])->get('/welcome');
        $this->assertEquals(['/test', '/welcome'], $this->app['session']->get('history'));
    }

}
