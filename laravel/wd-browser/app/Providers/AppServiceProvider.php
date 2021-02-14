<?php

namespace App\Providers;

use Illuminate\Support\ServiceProvider;
use Wikidata\Wikidata;

class AppServiceProvider extends ServiceProvider
{
    /**
     * Register any application services.
     *
     * @return void
     */
    public function register()
    {
        $this->app->bind(Wikidata::class, function ($app) {
            return new Wikidata();
        });
    }

    /**
     * Bootstrap any application services.
     *
     * @return void
     */
    public function boot()
    {
        //
    }
}
