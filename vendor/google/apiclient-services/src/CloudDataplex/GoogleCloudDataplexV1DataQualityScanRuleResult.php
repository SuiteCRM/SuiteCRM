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

class GoogleCloudDataplexV1DataQualityScanRuleResult extends \Google\Model
{
  /**
   * @var string
   */
  public $assertionRowCount;
  /**
   * @var string
   */
  public $column;
  /**
   * @var string
   */
  public $dataSource;
  /**
   * @var string
   */
  public $evaluatedRowCount;
  /**
   * @var string
   */
  public $evalutionType;
  /**
   * @var string
   */
  public $jobId;
  /**
   * @var string
   */
  public $nullRowCount;
  /**
   * @var string
   */
  public $passedRowCount;
  /**
   * @var string
   */
  public $result;
  /**
   * @var string
   */
  public $ruleDimension;
  /**
   * @var string
   */
  public $ruleName;
  /**
   * @var string
   */
  public $ruleType;
  public $thresholdPercent;

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
  public function setColumn($column)
  {
    $this->column = $column;
  }
  /**
   * @return string
   */
  public function getColumn()
  {
    return $this->column;
  }
  /**
   * @param string
   */
  public function setDataSource($dataSource)
  {
    $this->dataSource = $dataSource;
  }
  /**
   * @return string
   */
  public function getDataSource()
  {
    return $this->dataSource;
  }
  /**
   * @param string
   */
  public function setEvaluatedRowCount($evaluatedRowCount)
  {
    $this->evaluatedRowCount = $evaluatedRowCount;
  }
  /**
   * @return string
   */
  public function getEvaluatedRowCount()
  {
    return $this->evaluatedRowCount;
  }
  /**
   * @param string
   */
  public function setEvalutionType($evalutionType)
  {
    $this->evalutionType = $evalutionType;
  }
  /**
   * @return string
   */
  public function getEvalutionType()
  {
    return $this->evalutionType;
  }
  /**
   * @param string
   */
  public function setJobId($jobId)
  {
    $this->jobId = $jobId;
  }
  /**
   * @return string
   */
  public function getJobId()
  {
    return $this->jobId;
  }
  /**
   * @param string
   */
  public function setNullRowCount($nullRowCount)
  {
    $this->nullRowCount = $nullRowCount;
  }
  /**
   * @return string
   */
  public function getNullRowCount()
  {
    return $this->nullRowCount;
  }
  /**
   * @param string
   */
  public function setPassedRowCount($passedRowCount)
  {
    $this->passedRowCount = $passedRowCount;
  }
  /**
   * @return string
   */
  public function getPassedRowCount()
  {
    return $this->passedRowCount;
  }
  /**
   * @param string
   */
  public function setResult($result)
  {
    $this->result = $result;
  }
  /**
   * @return string
   */
  public function getResult()
  {
    return $this->result;
  }
  /**
   * @param string
   */
  public function setRuleDimension($ruleDimension)
  {
    $this->ruleDimension = $ruleDimension;
  }
  /**
   * @return string
   */
  public function getRuleDimension()
  {
    return $this->ruleDimension;
  }
  /**
   * @param string
   */
  public function setRuleName($ruleName)
  {
    $this->ruleName = $ruleName;
  }
  /**
   * @return string
   */
  public function getRuleName()
  {
    return $this->ruleName;
  }
  /**
   * @param string
   */
  public function setRuleType($ruleType)
  {
    $this->ruleType = $ruleType;
  }
  /**
   * @return string
   */
  public function getRuleType()
  {
    return $this->ruleType;
  }
  public function setThresholdPercent($thresholdPercent)
  {
    $this->thresholdPercent = $thresholdPercent;
  }
  public function getThresholdPercent()
  {
    return $this->thresholdPercent;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataQualityScanRuleResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataQualityScanRuleResult');
