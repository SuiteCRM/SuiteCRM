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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataQualityRuleResult extends \Google\Model
{
  /**
   * @var string
   */
  public $assertionRowCount;
  /**
   * @var string
   */
  public $evaluatedCount;
  /**
   * @var string
   */
  public $failingRowsQuery;
  /**
   * @var string
   */
  public $nullCount;
  public $passRatio;
  /**
   * @var bool
   */
  public $passed;
  /**
   * @var string
   */
  public $passedCount;
  protected $ruleType = GoogleCloudDataplexV1DataQualityRule::class;
  protected $ruleDataType = '';

  /**
   * @param string
   */
  public function setAssertionRowCount($assertionRowCount)
  {
    $this->assertionRowCount = $assertionRowCount;
  }
  /**
   * @return string
   */
  public function getAssertionRowCount()
  {
    return $this->assertionRowCount;
  }
  /**
   * @param string
   */
  public function setEvaluatedCount($evaluatedCount)
  {
    $this->evaluatedCount = $evaluatedCount;
  }
  /**
   * @return string
   */
  public function getEvaluatedCount()
  {
    return $this->evaluatedCount;
  }
  /**
   * @param string
   */
  public function setFailingRowsQuery($failingRowsQuery)
  {
    $this->failingRowsQuery = $failingRowsQuery;
  }
  /**
   * @return string
   */
  public function getFailingRowsQuery()
  {
    return $this->failingRowsQuery;
  }
  /**
   * @param string
   */
  public function setNullCount($nullCount)
  {
    $this->nullCount = $nullCount;
  }
  /**
   * @return string
   */
  public function getNullCount()
  {
    return $this->nullCount;
  }
  public function setPassRatio($passRatio)
  {
    $this->passRatio = $passRatio;
  }
  public function getPassRatio()
  {
    return $this->passRatio;
  }
  /**
   * @param bool
   */
  public function setPassed($passed)
  {
    $this->passed = $passed;
  }
  /**
   * @return bool
   */
  public function getPassed()
  {
    return $this->passed;
  }
  /**
   * @param string
   */
  public function setPassedCount($passedCount)
  {
    $this->passedCount = $passedCount;
  }
  /**
   * @return string
   */
  public function getPassedCount()
  {
    return $this->passedCount;
  }
  /**
   * @param GoogleCloudDataplexV1DataQualityRule
   */
  public function setRule(GoogleCloudDataplexV1DataQualityRule $rule)
  {
    $this->rule = $rule;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityRule
   */
  public function getRule()
  {
    return $this->rule;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataQualityRuleResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataQualityRuleResult');
