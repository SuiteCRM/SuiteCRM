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

namespace Google\Service\TagManager;

class ContainerFeatures extends \Google\Model
{
  /**
   * @var bool
   */
  public $supportBuiltInVariables;
  /**
   * @var bool
   */
  public $supportClients;
  /**
   * @var bool
   */
  public $supportEnvironments;
  /**
   * @var bool
   */
  public $supportFolders;
  /**
   * @var bool
   */
  public $supportGtagConfigs;
  /**
   * @var bool
   */
  public $supportTags;
  /**
   * @var bool
   */
  public $supportTemplates;
  /**
   * @var bool
   */
  public $supportTransformations;
  /**
   * @var bool
   */
  public $supportTriggers;
  /**
   * @var bool
   */
  public $supportUserPermissions;
  /**
   * @var bool
   */
  public $supportVariables;
  /**
   * @var bool
   */
  public $supportVersions;
  /**
   * @var bool
   */
  public $supportWorkspaces;
  /**
   * @var bool
   */
  public $supportZones;

  /**
   * @param bool
   */
  public function setSupportBuiltInVariables($supportBuiltInVariables)
  {
    $this->supportBuiltInVariables = $supportBuiltInVariables;
  }
  /**
   * @return bool
   */
  public function getSupportBuiltInVariables()
  {
    return $this->supportBuiltInVariables;
  }
  /**
   * @param bool
   */
  public function setSupportClients($supportClients)
  {
    $this->supportClients = $supportClients;
  }
  /**
   * @return bool
   */
  public function getSupportClients()
  {
    return $this->supportClients;
  }
  /**
   * @param bool
   */
  public function setSupportEnvironments($supportEnvironments)
  {
    $this->supportEnvironments = $supportEnvironments;
  }
  /**
   * @return bool
   */
  public function getSupportEnvironments()
  {
    return $this->supportEnvironments;
  }
  /**
   * @param bool
   */
  public function setSupportFolders($supportFolders)
  {
    $this->supportFolders = $supportFolders;
  }
  /**
   * @return bool
   */
  public function getSupportFolders()
  {
    return $this->supportFolders;
  }
  /**
   * @param bool
   */
  public function setSupportGtagConfigs($supportGtagConfigs)
  {
    $this->supportGtagConfigs = $supportGtagConfigs;
  }
  /**
   * @return bool
   */
  public function getSupportGtagConfigs()
  {
    return $this->supportGtagConfigs;
  }
  /**
   * @param bool
   */
  public function setSupportTags($supportTags)
  {
    $this->supportTags = $supportTags;
  }
  /**
   * @return bool
   */
  public function getSupportTags()
  {
    return $this->supportTags;
  }
  /**
   * @param bool
   */
  public function setSupportTemplates($supportTemplates)
  {
    $this->supportTemplates = $supportTemplates;
  }
  /**
   * @return bool
   */
  public function getSupportTemplates()
  {
    return $this->supportTemplates;
  }
  /**
   * @param bool
   */
  public function setSupportTransformations($supportTransformations)
  {
    $this->supportTransformations = $supportTransformations;
  }
  /**
   * @return bool
   */
  public function getSupportTransformations()
  {
    return $this->supportTransformations;
  }
  /**
   * @param bool
   */
  public function setSupportTriggers($supportTriggers)
  {
    $this->supportTriggers = $supportTriggers;
  }
  /**
   * @return bool
   */
  public function getSupportTriggers()
  {
    return $this->supportTriggers;
  }
  /**
   * @param bool
   */
  public function setSupportUserPermissions($supportUserPermissions)
  {
    $this->supportUserPermissions = $supportUserPermissions;
  }
  /**
   * @return bool
   */
  public function getSupportUserPermissions()
  {
    return $this->supportUserPermissions;
  }
  /**
   * @param bool
   */
  public function setSupportVariables($supportVariables)
  {
    $this->supportVariables = $supportVariables;
  }
  /**
   * @return bool
   */
  public function getSupportVariables()
  {
    return $this->supportVariables;
  }
  /**
   * @param bool
   */
  public function setSupportVersions($supportVersions)
  {
    $this->supportVersions = $supportVersions;
  }
  /**
   * @return bool
   */
  public function getSupportVersions()
  {
    return $this->supportVersions;
  }
  /**
   * @param bool
   */
  public function setSupportWorkspaces($supportWorkspaces)
  {
    $this->supportWorkspaces = $supportWorkspaces;
  }
  /**
   * @return bool
   */
  public function getSupportWorkspaces()
  {
    return $this->supportWorkspaces;
  }
  /**
   * @param bool
   */
  public function setSupportZones($supportZones)
  {
    $this->supportZones = $supportZones;
  }
  /**
   * @return bool
   */
  public function getSupportZones()
  {
    return $this->supportZones;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ContainerFeatures::class, 'Google_Service_TagManager_ContainerFeatures');
