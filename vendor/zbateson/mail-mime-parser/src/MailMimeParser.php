<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser;

use GuzzleHttp\Psr7\CachingStream;
use GuzzleHttp\Psr7\Utils;
use Psr\Http\Message\StreamInterface;
use ZBateson\MailMimeParser\Parser\MessageParser;

/**
 * Parses a MIME message into an {@see IMessage} object.
 *
 * The class sets up the Pimple dependency injection container with the ability
 * to override and/or provide specialized provider
 * {@see https://pimple.symfony.com/ \Pimple\ServiceProviderInterface}
 * classes to extend default classes used by MailMimeParser.
 *
 * To invoke, call parse on a MailMimeParser object.
 *
 * ```php
 * $parser = new MailMimeParser();
 * // the resource is attached due to the second parameter being true and will
 * // be closed when the returned IMessage is destroyed
 * $message = $parser->parse(fopen('path/to/file.txt'), true);
 * // use $message here
 * ```
 *
 * @author Zaahid Bateson
 */
class MailMimeParser
{
    /**
     * @var string the default charset used to encode strings (or string content
     *      like streams) returned by MailMimeParser (for e.g. the string
     *      returned by calling $message->getTextContent()).
     */
    public const DEFAULT_CHARSET = 'UTF-8';

    /**
     * @var Container dependency injection container
     */
    protected static $di = null;

    /**
     * @var MessageParser for parsing messages
     */
    protected $messageParser;

    /**
     * Returns the container.
     *
     * @return Container
     */
    public static function getDependencyContainer()
    {
        return static::$di;
    }

    /**
     * (Re)creates the container using the passed providers.
     *
     * This is necessary if configuration needs to be reset to parse emails
     * differently.
     *
     * Note that reconfiguring the dependency container can have an affect on
     * existing objects -- for instance if a provider were to override a
     * factory class, and an operation on an existing instance were to try to
     * create an object using that factory class, the new factory class would be
     * returned.  In other words, references to the Container are not maintained
     * in a non-static context separately, so care should be taken when
     * reconfiguring the parser.
     *
     * @param \Pimple\ServiceProviderInterface[] $providers
     */
    public static function configureDependencyContainer(array $providers = [])
    {
        static::$di = new Container();
        $di = static::$di;
        foreach ($providers as $provider) {
            $di->register($provider);
        }
    }

    /**
     * Override the dependency container completely.  If multiple configurations
     * are known to be needed, it would be better to keep the different
     * Container configurations and call setDependencyContainer instead of
     * {@see MailMimeParser::configureDependencyContainer}, which instantiates a
     * new {@see Container} on every call.
     *
     * @param Container $di
     */
    public static function setDependencyContainer(?Container $di = null)
    {
        static::$di = $di;
    }

    /**
     * Initializes the dependency container if not already initialized.
     *
     * To configure custom {@see https://pimple.symfony.com/ \Pimple\ServiceProviderInterface}
     * objects, call {@see MailMimeParser::configureDependencyContainer()}
     * before creating a MailMimeParser instance.
     */
    public function __construct()
    {
        if (static::$di === null) {
            static::configureDependencyContainer();
        }
        $di = static::$di;
        $this->messageParser = $di[\ZBateson\MailMimeParser\Parser\MessageParser::class];
    }

    /**
     * Parses the passed stream handle or string into an {@see IMessage} object
     * and returns it.
     *
     * If the passed $resource is a resource handle or StreamInterface, the
     * resource must remain open while the returned IMessage object exists.
     * Pass true as the second argument to have the resource attached to the
     * IMessage and closed for you when it's destroyed, or pass false to
     * manually close it if it should remain open after the IMessage object is
     * destroyed.
     *
     * @param resource|StreamInterface|string $resource The resource handle to
     *        the input stream of the mime message, or a string containing a
     *        mime message.
     * @param bool $attached pass true to have it attached to the returned
     *        IMessage and destroyed with it.
     * @return \ZBateson\MailMimeParser\IMessage
     */
    public function parse($resource, $attached)
    {
        $stream = Utils::streamFor(
            $resource,
            ['metadata' => ['mmp-detached-stream' => ($attached !== true)]]
        );
        if (!$stream->isSeekable()) {
            $stream = new CachingStream($stream);
        }
        return $this->messageParser->parse($stream);
    }
}
