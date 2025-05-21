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

namespace Google\Service\Backupdr;

class GcpBackupConfig extends \Google\Collection
{
  protected $collection_key = 'backupPlanRules';
  /**
   * @var string
   */
  public $backupPlan;
  /**
   * @var string
   */
  public $backupPlanAssociation;
  /**
   * @var string
   */
  public $backupPlanDescription;
  /**
   * @var string[]
   */
  public $backupPlanRules;

  /**
   * @param string
   */
  public function setBackupPlan($backupPlan)
  {
    $this->backupPlan = $backupPlan;
  }
  /**
   * @return string
   */
  public function getBackupPlan()
  {
    return $this->backupPlan;
  }
  /**
   * @param string
   */
  public function setBackupPlanAssociation($backupPlanAssociation)
  {
    $this->backupPlanAssociation = $backupPlanAssociation;
  }
  /**
   * @return string
   */
  public function getBackupPlanAssociation()
  {
    return $this->backupPlanAssociation;
  }
  /**
   * @param string
   */
  public function setBackupPlanDescription($backupPlanDescription)
  {
    $this->backupPlanDescription = $backupPlanDescription;
  }
  /**
   * @return string
   */
  public function getBackupPlanDescription()
  {
    return $this->backupPlanDescription;
  }
  /**
   * @param string[]
   */
  public function setBackupPlanRules($backupPlanRules)
  {
    $this->backupPlanRules = $backupPlanRules;
  }
  /**
   * @return string[]
   */
  public function getBackupPlanRules()
  {
    return $this->backupPlanRules;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GcpBackupConfig::class, 'Google_Service_Backupdr_GcpBackupConfig');
