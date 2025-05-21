<?php
namespace Api\V8\BeanDecorator;

#[\AllowDynamicProperties]
class BeanListRequest
{
    /**
     * @var \SugarBean
     */
    protected $bean;

    /**
     * @var string
     */
    protected $orderBy = '';

    /**
     * @var string
     */
    protected $where = '';

    /**
     * @var integer
     */
    protected $offset = BeanManager::DEFAULT_OFFSET;

    /**
     * @var integer
     */
    protected $limit = -1;

    /**
     * @var integer
     */
    protected $max = BeanManager::DEFAULT_ALL_RECORDS;

    /**
     * @var integer
     */
    protected $deleted = 0;

    /**
     * @var boolean
     */
    protected $singleSelect = true;

    /**
     * @var array
     */
    protected $fields = [];

    /**
     * @param \SugarBean $bean
     */
    public function __construct(\SugarBean $bean)
    {
        $this->bean = $bean;
    }

    /**
     * @param string $orderBy
     *
     * @return BeanListRequest
     */
    public function orderBy($orderBy)
    {
        $this->orderBy = $orderBy;

        return $this;
    }

    /**
     * @param string $where
     *
     * @return BeanListRequest
     */
    public function where($where)
    {
        $this->where = $where;

        return $this;
    }

    /**
     * @param integer $offset
     *
     * @return BeanListRequest
     */
    public function offset($offset)
    {
        $this->offset = $offset;

        return $this;
    }

    /**
     * @param integer $limit
     *
     * @return BeanListRequest
     */
    public function limit($limit)
    {
        $this->limit = $limit;

        return $this;
    }

    /**
     * @param integer $max
     *
     * @return BeanListRequest
     */
    public function max($max)
    {
        $this->max = $max;

        return $this;
    }

    /**
     * @param integer $deleted
     *
     * @return BeanListRequest
     */
    public function deleted($deleted)
    {
        $this->deleted = $deleted;

        return $this;
    }

    /**
     * @param boolean $singleSelect
     *
     * @return BeanListRequest
     */
    public function singleSelect($singleSelect)
    {
        $this->singleSelect = $singleSelect;

        return $this;
    }

    /**
     * @param array $fields
     *
     * @return BeanListRequest
     */
    public function fields(array $fields)
    {
        $this->fields = $fields;

        return $this;
    }

    /**
     * @return BeanListResponse
     */
    public function fetch()
    {
        return new BeanListResponse($this->bean->get_list(
            $this->orderBy,
            $this->where,
            $this->offset,
            $this->limit,
            $this->max,
            $this->deleted,
            $this->singleSelect,
            $this->fields
        ));
    }
}
