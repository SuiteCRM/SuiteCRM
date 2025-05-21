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

namespace Google\Service\ShoppingContent;

class TopicTrends extends \Google\Model
{
  /**
   * @var string
   */
  public $customerCountryCode;
  protected $dateType = Date::class;
  protected $dateDataType = '';
  public $last120DaysSearchInterest;
  public $last30DaysSearchInterest;
  public $last7DaysSearchInterest;
  public $last90DaysSearchInterest;
  public $next7DaysSearchInterest;
  public $searchInterest;
  /**
   * @var string
   */
  public $topic;

  /**
   * @param string
   */
  public function setCustomerCountryCode($customerCountryCode)
  {
    $this->customerCountryCode = $customerCountryCode;
  }
  /**
   * @return string
   */
  public function getCustomerCountryCode()
  {
    return $this->customerCountryCode;
  }
  /**
   * @param Date
   */
  public function setDate(Date $date)
  {
    $this->date = $date;
  }
  /**
   * @return Date
   */
  public function getDate()
  {
    return $this->date;
  }
  public function setLast120DaysSearchInterest($last120DaysSearchInterest)
  {
    $this->last120DaysSearchInterest = $last120DaysSearchInterest;
  }
  public function getLast120DaysSearchInterest()
  {
    return $this->last120DaysSearchInterest;
  }
  public function setLast30DaysSearchInterest($last30DaysSearchInterest)
  {
    $this->last30DaysSearchInterest = $last30DaysSearchInterest;
  }
  public function getLast30DaysSearchInterest()
  {
    return $this->last30DaysSearchInterest;
  }
  public function setLast7DaysSearchInterest($last7DaysSearchInterest)
  {
    $this->last7DaysSearchInterest = $last7DaysSearchInterest;
  }
  public function getLast7DaysSearchInterest()
  {
    return $this->last7DaysSearchInterest;
  }
  public function setLast90DaysSearchInterest($last90DaysSearchInterest)
  {
    $this->last90DaysSearchInterest = $last90DaysSearchInterest;
  }
  public function getLast90DaysSearchInterest()
  {
    return $this->last90DaysSearchInterest;
  }
  public function setNext7DaysSearchInterest($next7DaysSearchInterest)
  {
    $this->next7DaysSearchInterest = $next7DaysSearchInterest;
  }
  public function getNext7DaysSearchInterest()
  {
    return $this->next7DaysSearchInterest;
  }
  public function setSearchInterest($searchInterest)
  {
    $this->searchInterest = $searchInterest;
  }
  public function getSearchInterest()
  {
    return $this->searchInterest;
  }
  /**
   * @param string
   */
  public function setTopic($topic)
  {
    $this->topic = $topic;
  }
  /**
   * @return string
   */
  public function getTopic()
  {
    return $this->topic;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TopicTrends::class, 'Google_Service_ShoppingContent_TopicTrends');
