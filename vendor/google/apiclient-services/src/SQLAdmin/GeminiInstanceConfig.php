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

namespace Google\Service\SQLAdmin;

class GeminiInstanceConfig extends \Google\Model
{
  /**
   * @var bool
   */
  public $activeQueryEnabled;
  /**
   * @var bool
   */
  public $entitled;
  /**
   * @var bool
   */
  public $flagRecommenderEnabled;
  /**
   * @var bool
   */
  public $googleVacuumMgmtEnabled;
  /**
   * @var bool
   */
  public $indexAdvisorEnabled;
  /**
   * @var bool
   */
  public $oomSessionCancelEnabled;

  /**
   * @param bool
   */
  public function setActiveQueryEnabled($activeQueryEnabled)
  {
    $this->activeQueryEnabled = $activeQueryEnabled;
  }
  /**
   * @return bool
   */
  public function getActiveQueryEnabled()
  {
    return $this->activeQueryEnabled;
  }
  /**
   * @param bool
   */
  public function setEntitled($entitled)
  {
    $this->entitled = $entitled;
  }
  /**
   * @return bool
   */
  public function getEntitled()
  {
    return $this->entitled;
  }
  /**
   * @param bool
   */
  public function setFlagRecommenderEnabled($flagRecommenderEnabled)
  {
    $this->flagRecommenderEnabled = $flagRecommenderEnabled;
  }
  /**
   * @return bool
   */
  public function getFlagRecommenderEnabled()
  {
    return $this->flagRecommenderEnabled;
  }
  /**
   * @param bool
   */
  public function setGoogleVacuumMgmtEnabled($googleVacuumMgmtEnabled)
  {
    $this->googleVacuumMgmtEnabled = $googleVacuumMgmtEnabled;
  }
  /**
   * @return bool
   */
  public function getGoogleVacuumMgmtEnabled()
  {
    return $this->googleVacuumMgmtEnabled;
  }
  /**
   * @param bool
   */
  public function setIndexAdvisorEnabled($indexAdvisorEnabled)
  {
    $this->indexAdvisorEnabled = $indexAdvisorEnabled;
  }
  /**
   * @return bool
   */
  public function getIndexAdvisorEnabled()
  {
    return $this->indexAdvisorEnabled;
  }
  /**
   * @param bool
   */
  public function setOomSessionCancelEnabled($oomSessionCancelEnabled)
  {
    $this->oomSessionCancelEnabled = $oomSessionCancelEnabled;
  }
  /**
   * @return bool
   */
  public function getOomSessionCancelEnabled()
  {
    return $this->oomSessionCancelEnabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GeminiInstanceConfig::class, 'Google_Service_SQLAdmin_GeminiInstanceConfig');
