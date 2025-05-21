<?php

$clientId = getenv('GOOGLE_CLIENT_ID');
$clientSecret = getenv('GOOGLE_CLIENT_SECRET');
$redirectUri = 'http://localhost:8080';
$scopes = [ 'https://mail.google.com/' ];

$authUri = 'https://accounts.google.com/o/oauth2/v2/auth'
         . '?client_id=' . $clientId
         . '&scope=' . urlencode(implode(' ', $scopes))
         . '&redirect_uri=' . urlencode($redirectUri)
         . '&response_type=code'
         . '&prompt=consent'
         . '&access_type=offline';

$tokenUri = 'https://accounts.google.com/o/oauth2/token';

if (empty($_GET['code'])) {
    header('Location: '.$authUri);
    exit();
}

$postFields = 'client_id=' . $clientId
    . '&redirect_uri=' . urlencode($redirectUri)
    . '&client_secret=' . urlencode($clientSecret)
    . '&code=' . $_GET['code']
    . '&grant_type=authorization_code';

$curl = curl_init();
curl_setopt($curl, CURLOPT_URL, $tokenUri);
curl_setopt($curl, CURLOPT_RETURNTRANSFER, TRUE);
curl_setopt($curl, CURLOPT_POST, TRUE);
curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, FALSE);
curl_setopt($curl, CURLOPT_POSTFIELDS, $postFields);
$response = json_decode(curl_exec($curl), true);

echo '<pre>';
var_dump($response);
echo '</pre>';