@extends('layout')

@section('title', $entry->label)


{{--@section('sidebar')--}}
{{--    @parent--}}

{{--    <p>This is appended to the master sidebar.</p>--}}
{{--@endsection--}}

@section('content')
    <p>{{$entry->description}}</p>
    @if ($entry->wiki_url)
        <a href={{$entry->wiki_url}}>Wiki URL</a>
    @endif

    @if ($entry->aliases)
        <h2>Aliases</h2>
        <ul>
            @foreach ($entry->aliases as $alias)
                <li>{{$alias}}</li>
            @endforeach
        </ul>
    @endif

    @include('wd-properties-table', ['properties' => $entry->properties])
@endsection
