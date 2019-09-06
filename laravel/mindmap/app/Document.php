<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Document extends Model
{
    protected $table = 'mindmap_document';
    
    protected $guarded = ['id'];
}
