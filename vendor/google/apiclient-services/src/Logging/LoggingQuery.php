<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Logging;

class LoggingQuery extends \Google\Collection
{
  protected $collection_key = 'summaryFields';
  /**
   * @var string
   */
  public $filter;
  /**
   * @var int
   */
  public $summaryFieldEnd;
  /**
   * @var int
   */
  public $summaryFieldStart;
  protected $summaryFieldsType = SummaryField::class;
  protected $summaryFieldsDataType = 'array';

  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param int
   */
  public function setSummaryFieldEnd($summaryFieldEnd)
  {
    $this->summaryFieldEnd = $summaryFieldEnd;
  }
  /**
   * @return int
   */
  public function getSummaryFieldEnd()
  {
    return $this->summaryFieldEnd;
  }
  /**
   * @param int
   */
  public function setSummaryFieldStart($summaryFieldStart)
  {
    $this->summaryFieldStart = $summaryFieldStart;
  }
  /**
   * @return int
   */
  public function getSummaryFieldStart()
  {
    return $this->summaryFieldStart;
  }
  /**
   * @param SummaryField[]
   */
  public function setSummaryFields($summaryFields)
  {
    $this->summaryFields = $summaryFields;
  }
  /**
   * @return SummaryField[]
   */
  public function getSummaryFields()
  {
    return $this->summaryFields;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LoggingQuery::class, 'Google_Service_Logging_LoggingQuery');
