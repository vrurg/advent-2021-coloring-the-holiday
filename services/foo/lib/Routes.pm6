use Cro::HTTP::Router;

sub routes() is export {
    note "+ Establishing routes";
    route {
        get -> {
            note "-> Serving the content";
            content 'text/html', "<h1> foo </h1>";
        }
    }
}
