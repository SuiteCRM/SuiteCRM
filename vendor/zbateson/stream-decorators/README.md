# zbateson/stream-decorators

Psr7 stream decorators for character set conversion and common mail format content encodings.

[![Tests](https://github.com/zbateson/stream-decorators/actions/workflows/tests.yml/badge.svg)](https://github.com/zbateson/stream-decorators/actions/workflows/tests.yml)
[![Code Coverage](https://scrutinizer-ci.com/g/zbateson/stream-decorators/badges/coverage.png?b=master)](https://scrutinizer-ci.com/g/zbateson/stream-decorators/?branch=master)
[![Scrutinizer Code Quality](https://scrutinizer-ci.com/g/zbateson/stream-decorators/badges/quality-score.png?b=master)](https://scrutinizer-ci.com/g/zbateson/stream-decorators/?branch=master)
[![Total Downloads](https://poser.pugx.org/zbateson/stream-decorators/downloads)](//packagist.org/packages/zbateson/stream-decorators)
[![Latest Stable Version](https://poser.pugx.org/zbateson/stream-decorators/v)](//packagist.org/packages/zbateson/stream-decorators)

The goals of this project are to be:

* Well written
* Standards-compliant but forgiving
* Tested where possible

To include it for use in your project, please install via composer:

```
composer require zbateson/stream-decorators
```

## Requirements

StreamDecorators requires PHP 7.2 or newer.  Tested on PHP 7.2, 7.3, 7.4, 8.0, 8.1 and 8.2.

## Usage

```php
$stream = GuzzleHttp\Psr7\Utils::streamFor($handle);
$b64Stream = new ZBateson\StreamDecorators\Base64Stream($stream);
$charsetStream = new ZBateson\StreamDecorators\CharsetStream($b64Stream, 'UTF-32', 'UTF-8');

while (($line = GuzzleHttp\Psr7\Utils::readLine()) !== false) {
    echo $line, "\r\n";
}

```

Note that CharsetStream, depending on the target encoding, may return multiple bytes when a single 'char' is read.  If using php's 'fread', this will result in a warning:

'read x bytes more data than requested (xxxx read, xxxx max) - excess data will be lost

This is because the parameter to 'fread' is bytes, and so when CharsetStream returns, say, 4 bytes representing a single UTF-32 character, fread will truncate to the first byte when requesting '1' byte.  It is recommended to **not** convert to a stream handle (with StreamWrapper) for this reason when using CharsetStream.

The library consists of the following Psr\Http\Message\StreamInterface implementations:
* ZBateson\StreamDecorators\QuotedPrintableStream - decodes on read and encodes on write to quoted-printable
* ZBateson\StreamDecorators\Base64Stream - decodes on read and encodes on write to base64
* ZBateson\StreamDecorators\UUStream - decodes on read, encodes on write to uu-encoded
* ZBateson\StreamDecorators\CharsetStream - encodes from $streamCharset to $stringCharset on read, and vice-versa on write
* ZBateson\StreamDecorators\NonClosingStream - overrides close() and detach(), and simply unsets the attached stream without closing it
* ZBateson\StreamDecorators\ChunkSplitStream - splits written characters into lines of $lineLength long (stream implementation of php's chunk_split)
* ZBateson\StreamDecorators\PregReplaceFilterStream - calls preg_replace on with passed arguments on every read() call
* ZBateson\StreamDecorators\SeekingLimitStream - similar to GuzzleHttp's LimitStream, but maintains an internal current read position, seeking to it when read() is called, and seeking back to the wrapped stream's position after reading

QuotedPrintableStream, Base64Stream and UUStream's constructors take a single argument of a StreamInterface.
CharsetStreams's constructor also takes $streamCharset and $stringCharset as arguments respectively, ChunkSplitStream
optionally takes a $lineLength argument (defaults to 76) and a $lineEnding argument (defaults to CRLF).
PregReplaceFilterStream takes a $pattern argument and a $replacement argument.  SeekingLimitStream takes optional
$limit and $offset parameters, similar to GuzzleHttp's LimitStream.

## License

BSD licensed - please see [license agreement](https://github.com/zbateson/stream-decorators/blob/master/LICENSE).
