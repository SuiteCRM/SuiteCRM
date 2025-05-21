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

namespace Google\Service\CloudRun;

class GoogleCloudRunV2Volume extends \Google\Model
{
  protected $cloudSqlInstanceType = GoogleCloudRunV2CloudSqlInstance::class;
  protected $cloudSqlInstanceDataType = '';
  protected $emptyDirType = GoogleCloudRunV2EmptyDirVolumeSource::class;
  protected $emptyDirDataType = '';
  protected $gcsType = GoogleCloudRunV2GCSVolumeSource::class;
  protected $gcsDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $nfsType = GoogleCloudRunV2NFSVolumeSource::class;
  protected $nfsDataType = '';
  protected $secretType = GoogleCloudRunV2SecretVolumeSource::class;
  protected $secretDataType = '';

  /**
   * @param GoogleCloudRunV2CloudSqlInstance
   */
  public function setCloudSqlInstance(GoogleCloudRunV2CloudSqlInstance $cloudSqlInstance)
  {
    $this->cloudSqlInstance = $cloudSqlInstance;
  }
  /**
   * @return GoogleCloudRunV2CloudSqlInstance
   */
  public function getCloudSqlInstance()
  {
    return $this->cloudSqlInstance;
  }
  /**
   * @param GoogleCloudRunV2EmptyDirVolumeSource
   */
  public function setEmptyDir(GoogleCloudRunV2EmptyDirVolumeSource $emptyDir)
  {
    $this->emptyDir = $emptyDir;
  }
  /**
   * @return GoogleCloudRunV2EmptyDirVolumeSource
   */
  public function getEmptyDir()
  {
    return $this->emptyDir;
  }
  /**
   * @param GoogleCloudRunV2GCSVolumeSource
   */
  public function setGcs(GoogleCloudRunV2GCSVolumeSource $gcs)
  {
    $this->gcs = $gcs;
  }
  /**
   * @return GoogleCloudRunV2GCSVolumeSource
   */
  public function getGcs()
  {
    return $this->gcs;
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
   * @param GoogleCloudRunV2NFSVolumeSource
   */
  public function setNfs(GoogleCloudRunV2NFSVolumeSource $nfs)
  {
    $this->nfs = $nfs;
  }
  /**
   * @return GoogleCloudRunV2NFSVolumeSource
   */
  public function getNfs()
  {
    return $this->nfs;
  }
  /**
   * @param GoogleCloudRunV2SecretVolumeSource
   */
  public function setSecret(GoogleCloudRunV2SecretVolumeSource $secret)
  {
    $this->secret = $secret;
  }
  /**
   * @return GoogleCloudRunV2SecretVolumeSource
   */
  public function getSecret()
  {
    return $this->secret;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRunV2Volume::class, 'Google_Service_CloudRun_GoogleCloudRunV2Volume');
