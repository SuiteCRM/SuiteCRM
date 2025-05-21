<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MailMimeParser\Header\IHeaderPart;
use ZBateson\MbWrapper\MbWrapper;

/**
 * Constructs and returns IHeaderPart objects.
 *
 * @author Zaahid Bateson
 */
class HeaderPartFactory
{
    /**
     * @var MbWrapper $charsetConverter passed to IHeaderPart constructors
     *      for converting strings in IHeaderPart::convertEncoding
     */
    protected $charsetConverter;

    /**
     * Sets up dependencies.
     *
     */
    public function __construct(MbWrapper $charsetConverter)
    {
        $this->charsetConverter = $charsetConverter;
    }

    /**
     * Creates and returns a default IHeaderPart for this factory, allowing
     * subclass factories for specialized IHeaderParts.
     *
     * The default implementation returns a new Token.
     *
     * @return IHeaderPart
     */
    public function newInstance(string $value)
    {
        return $this->newToken($value);
    }

    /**
     * Initializes and returns a new Token.
     *
     * @return Token
     */
    public function newToken(string $value)
    {
        return new Token($this->charsetConverter, $value);
    }

    /**
     * Instantiates and returns a SplitParameterToken with the given name.
     *
     * @param string $name
     * @return SplitParameterToken
     */
    public function newSplitParameterToken($name)
    {
        return new SplitParameterToken($this->charsetConverter, $name);
    }

    /**
     * Initializes and returns a new LiteralPart.
     *
     * @param string $value
     * @return LiteralPart
     */
    public function newLiteralPart($value)
    {
        return new LiteralPart($this->charsetConverter, $value);
    }

    /**
     * Initializes and returns a new MimeLiteralPart.
     *
     * @param string $value
     * @return MimeLiteralPart
     */
    public function newMimeLiteralPart($value)
    {
        return new MimeLiteralPart($this->charsetConverter, $value);
    }

    /**
     * Initializes and returns a new CommentPart.
     *
     * @param string $value
     * @return CommentPart
     */
    public function newCommentPart($value)
    {
        return new CommentPart($this->charsetConverter, $value);
    }

    /**
     * Initializes and returns a new AddressPart.
     *
     * @param string $name
     * @param string $email
     * @return AddressPart
     */
    public function newAddressPart($name, $email)
    {
        return new AddressPart($this->charsetConverter, $name, $email);
    }

    /**
     * Initializes and returns a new AddressGroupPart
     *
     * @param string $name
     * @return AddressGroupPart
     */
    public function newAddressGroupPart(array $addresses, $name = '')
    {
        return new AddressGroupPart($this->charsetConverter, $addresses, $name);
    }

    /**
     * Initializes and returns a new DatePart
     *
     * @param string $value
     * @return DatePart
     */
    public function newDatePart($value)
    {
        return new DatePart($this->charsetConverter, $value);
    }

    /**
     * Initializes and returns a new ParameterPart.
     *
     * @param string $name
     * @param string $value
     * @param string $language
     * @return ParameterPart
     */
    public function newParameterPart($name, $value, $language = null)
    {
        return new ParameterPart($this->charsetConverter, $name, $value, $language);
    }

    /**
     * Initializes and returns a new ReceivedPart.
     *
     * @param string $name
     * @param string $value
     * @return ReceivedPart
     */
    public function newReceivedPart($name, $value)
    {
        return new ReceivedPart($this->charsetConverter, $name, $value);
    }

    /**
     * Initializes and returns a new ReceivedDomainPart.
     *
     * @param string $name
     * @param string $value
     * @param string $ehloName
     * @param string $hostName
     * @param string $hostAddress
     * @return ReceivedDomainPart
     */
    public function newReceivedDomainPart(
        $name,
        $value,
        $ehloName = null,
        $hostName = null,
        $hostAddress = null
    ) {
        return new ReceivedDomainPart(
            $this->charsetConverter,
            $name,
            $value,
            $ehloName,
            $hostName,
            $hostAddress
        );
    }
}
