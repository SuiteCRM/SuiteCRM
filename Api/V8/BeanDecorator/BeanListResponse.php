<?php
namespace Api\V8\BeanDecorator;

#[\AllowDynamicProperties]
class BeanListResponse
{
    /**
     * @var \SugarBean[]|[]
     */
    protected $beans;

    /**
     * @var int
     */
    protected $rowCount;

    /**
     * @param array $result
     */
    public function __construct(array $result = [])
    {
        $this->beans = isset($result['list']) ? $result['list'] : [];
        $this->rowCount = isset($result['row_count']) ? (int)$result['row_count'] : 0;
    }

    /**
     * @return \SugarBean[]|[]
     */
    public function getBeans()
    {
        return $this->beans;
    }

    /**
     * @return integer
     */
    public function getRowCount()
    {
        return $this->rowCount;
    }
}
