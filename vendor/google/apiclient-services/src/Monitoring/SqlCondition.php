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

namespace Google\Service\Monitoring;

class SqlCondition extends \Google\Model
{
  protected $booleanTestType = BooleanTest::class;
  protected $booleanTestDataType = '';
  protected $dailyType = Daily::class;
  protected $dailyDataType = '';
  protected $hourlyType = Hourly::class;
  protected $hourlyDataType = '';
  protected $minutesType = Minutes::class;
  protected $minutesDataType = '';
  /**
   * @var string
   */
  public $query;
  protected $rowCountTestType = RowCountTest::class;
  protected $rowCountTestDataType = '';

  /**
   * @param BooleanTest
   */
  public function setBooleanTest(BooleanTest $booleanTest)
  {
    $this->booleanTest = $booleanTest;
  }
  /**
   * @return BooleanTest
   */
  public function getBooleanTest()
  {
    return $this->booleanTest;
  }
  /**
   * @param Daily
   */
  public function setDaily(Daily $daily)
  {
    $this->daily = $daily;
  }
  /**
   * @return Daily
   */
  public function getDaily()
  {
    return $this->daily;
  }
  /**
   * @param Hourly
   */
  public function setHourly(Hourly $hourly)
  {
    $this->hourly = $hourly;
  }
  /**
   * @return Hourly
   */
  public function getHourly()
  {
    return $this->hourly;
  }
  /**
   * @param Minutes
   */
  public function setMinutes(Minutes $minutes)
  {
    $this->minutes = $minutes;
  }
  /**
   * @return Minutes
   */
  public function getMinutes()
  {
    return $this->minutes;
  }
  /**
   * @param string
   */
  public function setQuery($query)
  {
    $this->query = $query;
  }
  /**
   * @return string
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param RowCountTest
   */
  public function setRowCountTest(RowCountTest $rowCountTest)
  {
    $this->rowCountTest = $rowCountTest;
  }
  /**
   * @return RowCountTest
   */
  public function getRowCountTest()
  {
    return $this->rowCountTest;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlCondition::class, 'Google_Service_Monitoring_SqlCondition');
