<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxyFactory;

/**
 * Provides basic implementations for:
 * - IParser::setParserManager
 * - IParser::getParserMessageProxyFactory (returns $this->parserMessageProxyFactory
 *   which can be set via the default constructor)
 * - IParser::getParserPartProxyFactory (returns $this->parserPartProxyFactory
 *   which can be set via the default constructor)
 *
 * @author Zaahid Bateson
 */
abstract class AbstractParser implements IParser
{
    /**
     * @var ParserPartProxyFactory the parser's message proxy factory service
     *      responsible for creating an IMessage part wrapped in a
     *      ParserPartProxy.
     */
    protected $parserMessageProxyFactory;

    /**
     * @var ParserPartProxyFactory the parser's part proxy factory service
     *      responsible for creating IMessagePart parts wrapped in a
     *      ParserPartProxy.
     */
    protected $parserPartProxyFactory;

    /**
     * @var PartBuilderFactory Service for creating PartBuilder objects for new
     *      children.
     */
    protected $partBuilderFactory;

    /**
     * @var ParserManager the ParserManager, which should call setParserManager
     *      when the parser is added.
     */
    protected $parserManager;

    public function __construct(
        ParserPartProxyFactory $parserMessageProxyFactory,
        ParserPartProxyFactory $parserPartProxyFactory,
        PartBuilderFactory $partBuilderFactory
    ) {
        $this->parserMessageProxyFactory = $parserMessageProxyFactory;
        $this->parserPartProxyFactory = $parserPartProxyFactory;
        $this->partBuilderFactory = $partBuilderFactory;
    }

    /**
     * @return static
     */
    public function setParserManager(ParserManager $pm)
    {
        $this->parserManager = $pm;
        return $this;
    }

    public function getParserMessageProxyFactory()
    {
        return $this->parserMessageProxyFactory;
    }

    public function getParserPartProxyFactory()
    {
        return $this->parserPartProxyFactory;
    }
}
