<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use ZBateson\MailMimeParser\Parser\Proxy\ParserMimePartProxy;
use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxy;
use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxyFactory;

/**
 * Interface defining a message part parser.
 *
 * @author Zaahid Bateson
 */
interface IParser
{
    /**
     * Sets up the passed ParserManager as the ParserManager for this part,
     * which should be used when a new part is created (after its headers are
     * read and a PartBuilder is created from it.)
     *
     * @param ParserManager $pm The ParserManager to set.
     */
    public function setParserManager(ParserManager $pm);

    /**
     * Called by the ParserManager to determine if the passed PartBuilder is a
     * part handled by this IParser.
     */
    public function canParse(PartBuilder $part) : bool;

    /**
     * Returns the ParserPartProxyFactory responsible for creating IMessage
     * parts for this parser.
     *
     * This is called by ParserManager after 'canParse' if it returns true so
     * a ParserPartProxy can be created out of the PartBuilder.
     *
     * @return ParserPartProxyFactory
     */
    public function getParserMessageProxyFactory();

    /**
     * Returns the ParserPartProxyFactory responsible for creating IMessagePart
     * parts for this parser.
     *
     * This is called by ParserManager after 'canParse' if it returns true so
     * a ParserPartProxy can be created out of the PartBuilder.
     *
     * @return ParserPartProxyFactory
     */
    public function getParserPartProxyFactory();

    /**
     * Performs read operations for content from the stream of the passed
     * ParserPartProxy, and setting content bounds for the part in the passed
     * ParserPartProxy.
     *
     * The implementation should call $proxy->setStreamContentStartPos() and
     * $proxy->setStreamContentAndPartEndPos() so an IMessagePart can return
     * content from the raw message.
     *
     * Reading should stop once the end of the current part's content has been
     * reached or the end of the message has been reached.  If the end of the
     * message has been reached $proxy->setEof() should be called in addition to
     * setStreamContentAndPartEndPos().
     */
    public function parseContent(ParserPartProxy $proxy);

    /**
     * Performs read operations to read children from the passed $proxy, using
     * its stream, and reading up to (and not including) the beginning of the
     * child's content if another child exists.
     *
     * The implementation should:
     *  1. Return null if there are no more children.
     *  2. Read headers
     *  3. Create a PartBuilder (adding the passed $proxy as its parent)
     *  4. Call ParserManager::createParserProxyFor() on the ParserManager
     *     previously set by a call to setParserManager(), which may determine
     *     that a different parser is responsible for parts represented by
     *     the headers and PartBuilder passed to it.
     *
     * The method should then return the ParserPartProxy returned by the
     * ParserManager, or null if there are no more children to read.
     *
     * @return ParserPartProxy|null The child ParserPartProxy or null if there
     *         are no more children under $proxy.
     */
    public function parseNextChild(ParserMimePartProxy $proxy);
}
