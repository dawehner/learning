@foreach($session as $key => $value)
    <ul>
        <li>{{$key}} {{Arr::$value}}</li>
    </ul>
@endforeach
