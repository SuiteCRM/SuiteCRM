<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

use ZBateson\MailMimeParser\Header\Consumer\ConsumerService;
use ZBateson\MailMimeParser\Header\Part\MimeLiteralPartFactory;

/**
 * Constructs various IHeader types depending on the type of header passed.
 *
 * If the passed header resolves to a specific defined header type, it is parsed
 * as such.  Otherwise, a GenericHeader is instantiated and returned.  Headers
 * are mapped as follows:
 *
 *  - {@see AddressHeader}: From, To, Cc, Bcc, Sender, Reply-To, Resent-From,
 *    Resent-To, Resent-Cc, Resent-Bcc, Resent-Reply-To, Return-Path,
 *    Delivered-To
 *  - {@see DateHeader}: Date, Resent-Date, Delivery-Date, Expires, Expiry-Date,
 *    Reply-By
 *  - {@see ParameterHeader}: Content-Type, Content-Disposition, Received-SPF,
 *    Authentication-Results, DKIM-Signature, Autocrypt
 *  - {@see SubjectHeader}: Subject
 *  - {@see IdHeader}: Message-ID, Content-ID, In-Reply-To, References
 *  - {@see ReceivedHeader}: Received
 *
 * @author Zaahid Bateson
 */
class HeaderFactory
{
    /**
     * @var ConsumerService the passed ConsumerService providing
     *      AbstractConsumer singletons.
     */
    protected $consumerService;

    /**
     * @var MimeLiteralPartFactory for mime decoding.
     */
    protected $mimeLiteralPartFactory;

    /**
     * @var string[][] maps IHeader types to headers.
     */
    protected $types = [
        \ZBateson\MailMimeParser\Header\AddressHeader::class => [
            'from',
            'to',
            'cc',
            'bcc',
            'sender',
            'replyto',
            'resentfrom',
            'resentto',
            'resentcc',
            'resentbcc',
            'resentreplyto',
            'returnpath',
            'deliveredto',
        ],
        \ZBateson\MailMimeParser\Header\DateHeader::class => [
            'date',
            'resentdate',
            'deliverydate',
            'expires',
            'expirydate',
            'replyby',
        ],
        \ZBateson\MailMimeParser\Header\ParameterHeader::class => [
            'contenttype',
            'contentdisposition',
            'receivedspf',
            'authenticationresults',
            'dkimsignature',
            'autocrypt',
        ],
        \ZBateson\MailMimeParser\Header\SubjectHeader::class => [
            'subject',
        ],
        \ZBateson\MailMimeParser\Header\IdHeader::class => [
            'messageid',
            'contentid',
            'inreplyto',
            'references'
        ],
        \ZBateson\MailMimeParser\Header\ReceivedHeader::class => [
            'received'
        ]
    ];

    /**
     * @var string Defines the generic IHeader type to use for headers that
     *      aren't mapped in $types
     */
    protected $genericType = \ZBateson\MailMimeParser\Header\GenericHeader::class;

    /**
     * Instantiates member variables with the passed objects.
     *
     */
    public function __construct(ConsumerService $consumerService, MimeLiteralPartFactory $mimeLiteralPartFactory)
    {
        $this->consumerService = $consumerService;
        $this->mimeLiteralPartFactory = $mimeLiteralPartFactory;
    }

    /**
     * Returns the string in lower-case, and with non-alphanumeric characters
     * stripped out.
     *
     * @param string $header The header name
     * @return string The normalized header name
     */
    public function getNormalizedHeaderName(string $header) : string
    {
        return \preg_replace('/[^a-z0-9]/', '', \strtolower($header));
    }

    /**
     * Returns the name of an IHeader class for the passed header name.
     *
     * @param string $name The header name.
     * @return string The Fully Qualified class name.
     */
    private function getClassFor(string $name) : string
    {
        $test = $this->getNormalizedHeaderName($name);
        foreach ($this->types as $class => $matchers) {
            foreach ($matchers as $matcher) {
                if ($test === $matcher) {
                    return $class;
                }
            }
        }
        return $this->genericType;
    }

    /**
     * Creates an IHeader instance for the passed header name and value, and
     * returns it.
     *
     * @param string $name The header name.
     * @param string $value The header value.
     * @return IHeader The created header object.
     */
    public function newInstance(string $name, string $value)
    {
        $class = $this->getClassFor($name);
        return $this->newInstanceOf($name, $value, $class);
    }

    /**
     * Creates an IHeader instance for the passed header name and value, and
     * returns it.
     *
     * @param string $name The header name.
     * @param string $value The header value.
     * @return IHeader The created header object.
     */
    public function newInstanceOf(string $name, string $value, string $iHeaderClass) : IHeader
    {
        if (\is_a($iHeaderClass, 'ZBateson\MailMimeParser\Header\MimeEncodedHeader', true)) {
            return new $iHeaderClass(
                $this->mimeLiteralPartFactory,
                $this->consumerService,
                $name,
                $value
            );
        }
        return new $iHeaderClass($this->consumerService, $name, $value);
    }
}
