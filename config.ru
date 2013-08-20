use Rack::Static, 
  :urls => ["/js", "/css", "/images"],
  :root => "resources/public"

run lambda { |env|
  uri = env['REQUEST_URI']
  base_dir = 'resources/public/'
  file = base_dir + (uri =~ /dots/ ? 'dots.html' : '')
  [
    200, 
    {
      'Content-Type'  => 'text/html', 
      'Cache-Control' => 'public, max-age=86400' 
   },
   File.open(file, File::RDONLY)
  ]
}
