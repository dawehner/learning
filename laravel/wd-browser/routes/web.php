<?php

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;

use Wikidata\Wikidata;

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| Here is where you can register web routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| contains the "web" middleware group. Now create something great!
|
*/

Route::get(
    '/',
    function (Wikidata $wd) {
        $results = $wd->search('kingston', 'en', 5);

        return view('wd-list', ['results' => $results]);
    }
);

Route::get(
    '/entity/{eid}',
    function (string $eid, Request $request, Wikidata $wd) {
        $res = $wd->get($eid);

        return view('wd-entry-page',
                    [
                        'entry' => $res,
                        'session' => app('session'),
                    ]
        );
    }
);

Route::get(
    '/search',
    function (Request $request, Wikidata $wd) {
        $results = $wd->search($request->query->get('term'), 'en', 5);
        return view('wd-list', ['results' => $results]);
    }
);

Route::get(
    '/welcome',
    function () {
        return view('welcome');
    }
);
