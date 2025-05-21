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

namespace Google\Service\ContainerAnalysis;

class ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsGoModule extends \Google\Model
{
  /**
   * @var string
   */
  public $modulePath;
  /**
   * @var string
   */
  public $moduleVersion;
  /**
   * @var string
   */
  public $repositoryLocation;
  /**
   * @var string
   */
  public $repositoryName;
  /**
   * @var string
   */
  public $repositoryProjectId;
  /**
   * @var string
   */
  public $sourcePath;

  /**
   * @param string
   */
  public function setModulePath($modulePath)
  {
    $this->modulePath = $modulePath;
  }
  /**
   * @return string
   */
  public function getModulePath()
  {
    return $this->modulePath;
  }
  /**
   * @param string
   */
  public function setModuleVersion($moduleVersion)
  {
    $this->moduleVersion = $moduleVersion;
  }
  /**
   * @return string
   */
  public function getModuleVersion()
  {
    return $this->moduleVersion;
  }
  /**
   * @param string
   */
  public function setRepositoryLocation($repositoryLocation)
  {
    $this->repositoryLocation = $repositoryLocation;
  }
  /**
   * @return string
   */
  public function getRepositoryLocation()
  {
    return $this->repositoryLocation;
  }
  /**
   * @param string
   */
  public function setRepositoryName($repositoryName)
  {
    $this->repositoryName = $repositoryName;
  }
  /**
   * @return string
   */
  public function getRepositoryName()
  {
    return $this->repositoryName;
  }
  /**
   * @param string
   */
  public function setRepositoryProjectId($repositoryProjectId)
  {
    $this->repositoryProjectId = $repositoryProjectId;
  }
  /**
   * @return string
   */
  public function getRepositoryProjectId()
  {
    return $this->repositoryProjectId;
  }
  /**
   * @param string
   */
  public function setSourcePath($sourcePath)
  {
    $this->sourcePath = $sourcePath;
  }
  /**
   * @return string
   */
  public function getSourcePath()
  {
    return $this->sourcePath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsGoModule::class, 'Google_Service_ContainerAnalysis_ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsGoModule');
