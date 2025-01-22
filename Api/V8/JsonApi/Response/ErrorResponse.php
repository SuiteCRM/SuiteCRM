<?php
namespace Api\V8\JsonApi\Response;

use Api\Core\Config\ApiConfig;
use Exception;
use JsonSerializable;

#[\AllowDynamicProperties]
class ErrorResponse implements JsonSerializable
{
    /**
     * @var integer
     */
    private $status;

    /**
     * @var string
     */
    private $title;

    /**
     * @var string
     */
    private $detail;
    
    /**
     *
     * @var Exception
     */
    private $exception;
    
    /**
     * In debug mode, ErrorResponse should shows full description about occurred exceptions.
     *
     * @todo  documentation needs to be updated at this point (about debug exceptions)
     * @var boolean
     */
    protected $debugExceptions;
    
    /**
     *
     * @param bool|null $debugExceptions optional - using ApiConfig setting by default
     */
    public function __construct($debugExceptions = null)
    {
        $this->debugExceptions =
            null === $debugExceptions ?
                ApiConfig::getDebugExceptions() :
                $debugExceptions;
    }

    /**
     * @return integer
     */
    public function getStatus()
    {
        return $this->status;
    }

    /**
     * @param integer $status
     */
    public function setStatus($status)
    {
        $this->status = $status;
    }

    /**
     * @return string
     */
    public function getTitle()
    {
        return $this->title;
    }

    /**
     * @param string $title
     */
    public function setTitle($title)
    {
        $this->title = $title;
    }

    /**
     * @return string
     */
    public function getDetail()
    {
        return $this->detail;
    }

    /**
     * @param string $detail
     */
    public function setDetail($detail)
    {
        $this->detail = $detail;
    }
    
    /**
     *
     * @param Exception $exception
     */
    public function setException(Exception $exception)
    {
        $this->exception = $exception;
    }
    
    /**
     *
     * @param Exception $exception
     * @return array
     */
    protected static function exceptionToArray(Exception $exception)
    {
        return [
            'code' => $exception->getCode(),
            'file' => $exception->getFile(),
            'line' => $exception->getLine(),
            'message' => $exception->getMessage(),
            'previous' => self::exceptionToArray($exception->getPrevious()),
            'trace' => $exception->getTrace(),
            'traceAsString' => $exception->getTraceAsString(),
        ];
    }
    
    /**
     *
     * @return array
     */
    public function getExceptionArray()
    {
        if (!$this->exception) {
            return null;
        }
        return self::exceptionToArray($this->exception);
    }

    /**
     * @inheritdoc
     */
    public function jsonSerialize()
    {
        $ret = [
            'errors' => [
                'status' => $this->getStatus(),
                'title' => $this->getTitle(),
                'detail' => $this->getDetail(),
            ]
        ];
        
        // do it only in debug mode!!!!
        if ($this->debugExceptions) {
            $ret['errors']['exception'] = $this->getExceptionArray();
        }
        
        return $ret;
    }
}
