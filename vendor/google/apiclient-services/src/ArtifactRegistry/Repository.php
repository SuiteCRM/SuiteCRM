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

namespace Google\Service\ArtifactRegistry;

class Repository extends \Google\Model
{
  protected $cleanupPoliciesType = CleanupPolicy::class;
  protected $cleanupPoliciesDataType = 'map';
  /**
   * @var bool
   */
  public $cleanupPolicyDryRun;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $disallowUnspecifiedMode;
  protected $dockerConfigType = DockerRepositoryConfig::class;
  protected $dockerConfigDataType = '';
  /**
   * @var string
   */
  public $format;
  /**
   * @var string
   */
  public $kmsKeyName;
  /**
   * @var string[]
   */
  public $labels;
  protected $mavenConfigType = MavenRepositoryConfig::class;
  protected $mavenConfigDataType = '';
  /**
   * @var string
   */
  public $mode;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $registryUri;
  protected $remoteRepositoryConfigType = RemoteRepositoryConfig::class;
  protected $remoteRepositoryConfigDataType = '';
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $sizeBytes;
  /**
   * @var string
   */
  public $updateTime;
  protected $virtualRepositoryConfigType = VirtualRepositoryConfig::class;
  protected $virtualRepositoryConfigDataType = '';
  protected $vulnerabilityScanningConfigType = VulnerabilityScanningConfig::class;
  protected $vulnerabilityScanningConfigDataType = '';

  /**
   * @param CleanupPolicy[]
   */
  public function setCleanupPolicies($cleanupPolicies)
  {
    $this->cleanupPolicies = $cleanupPolicies;
  }
  /**
   * @return CleanupPolicy[]
   */
  public function getCleanupPolicies()
  {
    return $this->cleanupPolicies;
  }
  /**
   * @param bool
   */
  public function setCleanupPolicyDryRun($cleanupPolicyDryRun)
  {
    $this->cleanupPolicyDryRun = $cleanupPolicyDryRun;
  }
  /**
   * @return bool
   */
  public function getCleanupPolicyDryRun()
  {
    return $this->cleanupPolicyDryRun;
  }
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
   * @param bool
   */
  public function setDisallowUnspecifiedMode($disallowUnspecifiedMode)
  {
    $this->disallowUnspecifiedMode = $disallowUnspecifiedMode;
  }
  /**
   * @return bool
   */
  public function getDisallowUnspecifiedMode()
  {
    return $this->disallowUnspecifiedMode;
  }
  /**
   * @param DockerRepositoryConfig
   */
  public function setDockerConfig(DockerRepositoryConfig $dockerConfig)
  {
    $this->dockerConfig = $dockerConfig;
  }
  /**
   * @return DockerRepositoryConfig
   */
  public function getDockerConfig()
  {
    return $this->dockerConfig;
  }
  /**
   * @param string
   */
  public function setFormat($format)
  {
    $this->format = $format;
  }
  /**
   * @return string
   */
  public function getFormat()
  {
    return $this->format;
  }
  /**
   * @param string
   */
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
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
   * @param MavenRepositoryConfig
   */
  public function setMavenConfig(MavenRepositoryConfig $mavenConfig)
  {
    $this->mavenConfig = $mavenConfig;
  }
  /**
   * @return MavenRepositoryConfig
   */
  public function getMavenConfig()
  {
    return $this->mavenConfig;
  }
  /**
   * @param string
   */
  public function setMode($mode)
  {
    $this->mode = $mode;
  }
  /**
   * @return string
   */
  public function getMode()
  {
    return $this->mode;
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
   * @param string
   */
  public function setRegistryUri($registryUri)
  {
    $this->registryUri = $registryUri;
  }
  /**
   * @return string
   */
  public function getRegistryUri()
  {
    return $this->registryUri;
  }
  /**
   * @param RemoteRepositoryConfig
   */
  public function setRemoteRepositoryConfig(RemoteRepositoryConfig $remoteRepositoryConfig)
  {
    $this->remoteRepositoryConfig = $remoteRepositoryConfig;
  }
  /**
   * @return RemoteRepositoryConfig
   */
  public function getRemoteRepositoryConfig()
  {
    return $this->remoteRepositoryConfig;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param string
   */
  public function setSizeBytes($sizeBytes)
  {
    $this->sizeBytes = $sizeBytes;
  }
  /**
   * @return string
   */
  public function getSizeBytes()
  {
    return $this->sizeBytes;
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
  /**
   * @param VirtualRepositoryConfig
   */
  public function setVirtualRepositoryConfig(VirtualRepositoryConfig $virtualRepositoryConfig)
  {
    $this->virtualRepositoryConfig = $virtualRepositoryConfig;
  }
  /**
   * @return VirtualRepositoryConfig
   */
  public function getVirtualRepositoryConfig()
  {
    return $this->virtualRepositoryConfig;
  }
  /**
   * @param VulnerabilityScanningConfig
   */
  public function setVulnerabilityScanningConfig(VulnerabilityScanningConfig $vulnerabilityScanningConfig)
  {
    $this->vulnerabilityScanningConfig = $vulnerabilityScanningConfig;
  }
  /**
   * @return VulnerabilityScanningConfig
   */
  public function getVulnerabilityScanningConfig()
  {
    return $this->vulnerabilityScanningConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Repository::class, 'Google_Service_ArtifactRegistry_Repository');
