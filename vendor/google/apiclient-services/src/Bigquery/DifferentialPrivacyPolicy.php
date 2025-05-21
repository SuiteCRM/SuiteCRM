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

namespace Google\Service\Bigquery;

class DifferentialPrivacyPolicy extends \Google\Model
{
  public $deltaBudget;
  public $deltaBudgetRemaining;
  public $deltaPerQuery;
  public $epsilonBudget;
  public $epsilonBudgetRemaining;
  public $maxEpsilonPerQuery;
  /**
   * @var string
   */
  public $maxGroupsContributed;
  /**
   * @var string
   */
  public $privacyUnitColumn;

  public function setDeltaBudget($deltaBudget)
  {
    $this->deltaBudget = $deltaBudget;
  }
  public function getDeltaBudget()
  {
    return $this->deltaBudget;
  }
  public function setDeltaBudgetRemaining($deltaBudgetRemaining)
  {
    $this->deltaBudgetRemaining = $deltaBudgetRemaining;
  }
  public function getDeltaBudgetRemaining()
  {
    return $this->deltaBudgetRemaining;
  }
  public function setDeltaPerQuery($deltaPerQuery)
  {
    $this->deltaPerQuery = $deltaPerQuery;
  }
  public function getDeltaPerQuery()
  {
    return $this->deltaPerQuery;
  }
  public function setEpsilonBudget($epsilonBudget)
  {
    $this->epsilonBudget = $epsilonBudget;
  }
  public function getEpsilonBudget()
  {
    return $this->epsilonBudget;
  }
  public function setEpsilonBudgetRemaining($epsilonBudgetRemaining)
  {
    $this->epsilonBudgetRemaining = $epsilonBudgetRemaining;
  }
  public function getEpsilonBudgetRemaining()
  {
    return $this->epsilonBudgetRemaining;
  }
  public function setMaxEpsilonPerQuery($maxEpsilonPerQuery)
  {
    $this->maxEpsilonPerQuery = $maxEpsilonPerQuery;
  }
  public function getMaxEpsilonPerQuery()
  {
    return $this->maxEpsilonPerQuery;
  }
  /**
   * @param string
   */
  public function setMaxGroupsContributed($maxGroupsContributed)
  {
    $this->maxGroupsContributed = $maxGroupsContributed;
  }
  /**
   * @return string
   */
  public function getMaxGroupsContributed()
  {
    return $this->maxGroupsContributed;
  }
  /**
   * @param string
   */
  public function setPrivacyUnitColumn($privacyUnitColumn)
  {
    $this->privacyUnitColumn = $privacyUnitColumn;
  }
  /**
   * @return string
   */
  public function getPrivacyUnitColumn()
  {
    return $this->privacyUnitColumn;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DifferentialPrivacyPolicy::class, 'Google_Service_Bigquery_DifferentialPrivacyPolicy');
