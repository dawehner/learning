@if (Str::startsWith($ref, 'Q'))
    <a href="/entity/{{$ref}}">{{$title}}</a>
@else
    {{$title}}
@endif
