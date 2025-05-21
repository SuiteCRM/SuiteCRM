<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Part;

use ZBateson\MailMimeParser\Header\HeaderFactory;

/**
 * Creates UUEncodedPartHeaderContainer instances.
 *
 * @author Zaahid Bateson
 */
class UUEncodedPartHeaderContainerFactory
{
    /**
     * @var HeaderFactory the HeaderFactory passed to
     *      UUEncodedPartHeaderContainer instances.
     */
    protected $headerFactory;

    /**
     * Constructor
     *
     */
    public function __construct(HeaderFactory $headerFactory)
    {
        $this->headerFactory = $headerFactory;
    }

    /**
     * Creates and returns a UUEncodedPartHeaderContainer.
     *
     * @return UUEncodedPartHeaderContainer
     */
    public function newInstance(int $mode, string $filename)
    {
        $container = new UUEncodedPartHeaderContainer($this->headerFactory);
        $container->setUnixFileMode($mode);
        $container->setFilename($filename);
        return $container;
    }
}
