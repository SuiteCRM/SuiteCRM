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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaTestCase extends \Google\Collection
{
  protected $collection_key = 'testTaskConfigs';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $creatorEmail;
  /**
   * @var string
   */
  public $databasePersistencePolicy;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $lastModifierEmail;
  /**
   * @var string
   */
  public $lockHolderEmail;
  /**
   * @var string
   */
  public $name;
  protected $testInputParametersType = GoogleCloudIntegrationsV1alphaIntegrationParameter::class;
  protected $testInputParametersDataType = 'array';
  protected $testTaskConfigsType = GoogleCloudIntegrationsV1alphaTestTaskConfig::class;
  protected $testTaskConfigsDataType = 'array';
  protected $triggerConfigType = GoogleCloudIntegrationsV1alphaTriggerConfig::class;
  protected $triggerConfigDataType = '';
  /**
   * @var string
   */
  public $triggerId;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setCreatorEmail($creatorEmail)
  {
    $this->creatorEmail = $creatorEmail;
  }
  /**
   * @return string
   */
  public function getCreatorEmail()
  {
    return $this->creatorEmail;
  }
  /**
   * @param string
   */
  public function setDatabasePersistencePolicy($databasePersistencePolicy)
  {
    $this->databasePersistencePolicy = $databasePersistencePolicy;
  }
  /**
   * @return string
   */
  public function getDatabasePersistencePolicy()
  {
    return $this->databasePersistencePolicy;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setLastModifierEmail($lastModifierEmail)
  {
    $this->lastModifierEmail = $lastModifierEmail;
  }
  /**
   * @return string
   */
  public function getLastModifierEmail()
  {
    return $this->lastModifierEmail;
  }
  /**
   * @param string
   */
  public function setLockHolderEmail($lockHolderEmail)
  {
    $this->lockHolderEmail = $lockHolderEmail;
  }
  /**
   * @return string
   */
  public function getLockHolderEmail()
  {
    return $this->lockHolderEmail;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaIntegrationParameter[]
   */
  public function setTestInputParameters($testInputParameters)
  {
    $this->testInputParameters = $testInputParameters;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaIntegrationParameter[]
   */
  public function getTestInputParameters()
  {
    return $this->testInputParameters;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaTestTaskConfig[]
   */
  public function setTestTaskConfigs($testTaskConfigs)
  {
    $this->testTaskConfigs = $testTaskConfigs;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaTestTaskConfig[]
   */
  public function getTestTaskConfigs()
  {
    return $this->testTaskConfigs;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaTriggerConfig
   */
  public function setTriggerConfig(GoogleCloudIntegrationsV1alphaTriggerConfig $triggerConfig)
  {
    $this->triggerConfig = $triggerConfig;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaTriggerConfig
   */
  public function getTriggerConfig()
  {
    return $this->triggerConfig;
  }
  /**
   * @param string
   */
  public function setTriggerId($triggerId)
  {
    $this->triggerId = $triggerId;
  }
  /**
   * @return string
   */
  public function getTriggerId()
  {
    return $this->triggerId;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaTestCase::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaTestCase');
