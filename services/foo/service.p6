use Cro::HTTP::Log::File;
use Cro::HTTP::Server;
use Routes;

$*OUT.out-buffer = 0;
$*ERR.out-buffer = 0;

my Cro::Service $http = Cro::HTTP::Server.new(
    http => <1.1>,
    host => %*ENV<FOO_HOST> ||
        die("Missing FOO_HOST in environment"),
    port => %*ENV<FOO_PORT> ||
        die("Missing FOO_PORT in environment"),
    application => routes(),
    after => [
        Cro::HTTP::Log::File.new(logs => $*OUT, errors => $*ERR)
    ]
);
$http.start;
say "Listening at http://%*ENV<FOO_HOST>:%*ENV<FOO_PORT>";
react {
    whenever signal(SIGINT) {
        say "Shutting down...";
        $http.stop;
        done;
    }
}
