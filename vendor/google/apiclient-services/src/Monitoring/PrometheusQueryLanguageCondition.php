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

class PrometheusQueryLanguageCondition extends \Google\Model
{
  /**
   * @var string
   */
  public $alertRule;
  /**
   * @var bool
   */
  public $disableMetricValidation;
  /**
   * @var string
   */
  public $duration;
  /**
   * @var string
   */
  public $evaluationInterval;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $query;
  /**
   * @var string
   */
  public $ruleGroup;

  /**
   * @param string
   */
  public function setAlertRule($alertRule)
  {
    $this->alertRule = $alertRule;
  }
  /**
   * @return string
   */
  public function getAlertRule()
  {
    return $this->alertRule;
  }
  /**
   * @param bool
   */
  public function setDisableMetricValidation($disableMetricValidation)
  {
    $this->disableMetricValidation = $disableMetricValidation;
  }
  /**
   * @return bool
   */
  public function getDisableMetricValidation()
  {
    return $this->disableMetricValidation;
  }
  /**
   * @param string
   */
  public function setDuration($duration)
  {
    $this->duration = $duration;
  }
  /**
   * @return string
   */
  public function getDuration()
  {
    return $this->duration;
  }
  /**
   * @param string
   */
  public function setEvaluationInterval($evaluationInterval)
  {
    $this->evaluationInterval = $evaluationInterval;
  }
  /**
   * @return string
   */
  public function getEvaluationInterval()
  {
    return $this->evaluationInterval;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
   * @param string
   */
  public function setRuleGroup($ruleGroup)
  {
    $this->ruleGroup = $ruleGroup;
  }
  /**
   * @return string
   */
  public function getRuleGroup()
  {
    return $this->ruleGroup;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PrometheusQueryLanguageCondition::class, 'Google_Service_Monitoring_PrometheusQueryLanguageCondition');
