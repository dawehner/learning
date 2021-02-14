@extends('layout')

@section('title', 'Test')

{{--@section('sidebar')--}}
{{--    @parent--}}

{{--    <p>This is appended to the master sidebar.</p>--}}
{{--@endsection--}}

@section('content')
    @include('wd-properties-table', ['properties' => $entry->properties])
@endsection
