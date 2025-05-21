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

namespace Google\Service\DLP;

class GooglePrivacyDlpV2DiscoveryTarget extends \Google\Model
{
  protected $bigQueryTargetType = GooglePrivacyDlpV2BigQueryDiscoveryTarget::class;
  protected $bigQueryTargetDataType = '';
  protected $cloudSqlTargetType = GooglePrivacyDlpV2CloudSqlDiscoveryTarget::class;
  protected $cloudSqlTargetDataType = '';
  protected $cloudStorageTargetType = GooglePrivacyDlpV2CloudStorageDiscoveryTarget::class;
  protected $cloudStorageTargetDataType = '';
  protected $otherCloudTargetType = GooglePrivacyDlpV2OtherCloudDiscoveryTarget::class;
  protected $otherCloudTargetDataType = '';
  protected $secretsTargetType = GooglePrivacyDlpV2SecretsDiscoveryTarget::class;
  protected $secretsTargetDataType = '';
  protected $vertexDatasetTargetType = GooglePrivacyDlpV2VertexDatasetDiscoveryTarget::class;
  protected $vertexDatasetTargetDataType = '';

  /**
   * @param GooglePrivacyDlpV2BigQueryDiscoveryTarget
   */
  public function setBigQueryTarget(GooglePrivacyDlpV2BigQueryDiscoveryTarget $bigQueryTarget)
  {
    $this->bigQueryTarget = $bigQueryTarget;
  }
  /**
   * @return GooglePrivacyDlpV2BigQueryDiscoveryTarget
   */
  public function getBigQueryTarget()
  {
    return $this->bigQueryTarget;
  }
  /**
   * @param GooglePrivacyDlpV2CloudSqlDiscoveryTarget
   */
  public function setCloudSqlTarget(GooglePrivacyDlpV2CloudSqlDiscoveryTarget $cloudSqlTarget)
  {
    $this->cloudSqlTarget = $cloudSqlTarget;
  }
  /**
   * @return GooglePrivacyDlpV2CloudSqlDiscoveryTarget
   */
  public function getCloudSqlTarget()
  {
    return $this->cloudSqlTarget;
  }
  /**
   * @param GooglePrivacyDlpV2CloudStorageDiscoveryTarget
   */
  public function setCloudStorageTarget(GooglePrivacyDlpV2CloudStorageDiscoveryTarget $cloudStorageTarget)
  {
    $this->cloudStorageTarget = $cloudStorageTarget;
  }
  /**
   * @return GooglePrivacyDlpV2CloudStorageDiscoveryTarget
   */
  public function getCloudStorageTarget()
  {
    return $this->cloudStorageTarget;
  }
  /**
   * @param GooglePrivacyDlpV2OtherCloudDiscoveryTarget
   */
  public function setOtherCloudTarget(GooglePrivacyDlpV2OtherCloudDiscoveryTarget $otherCloudTarget)
  {
    $this->otherCloudTarget = $otherCloudTarget;
  }
  /**
   * @return GooglePrivacyDlpV2OtherCloudDiscoveryTarget
   */
  public function getOtherCloudTarget()
  {
    return $this->otherCloudTarget;
  }
  /**
   * @param GooglePrivacyDlpV2SecretsDiscoveryTarget
   */
  public function setSecretsTarget(GooglePrivacyDlpV2SecretsDiscoveryTarget $secretsTarget)
  {
    $this->secretsTarget = $secretsTarget;
  }
  /**
   * @return GooglePrivacyDlpV2SecretsDiscoveryTarget
   */
  public function getSecretsTarget()
  {
    return $this->secretsTarget;
  }
  /**
   * @param GooglePrivacyDlpV2VertexDatasetDiscoveryTarget
   */
  public function setVertexDatasetTarget(GooglePrivacyDlpV2VertexDatasetDiscoveryTarget $vertexDatasetTarget)
  {
    $this->vertexDatasetTarget = $vertexDatasetTarget;
  }
  /**
   * @return GooglePrivacyDlpV2VertexDatasetDiscoveryTarget
   */
  public function getVertexDatasetTarget()
  {
    return $this->vertexDatasetTarget;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryTarget::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryTarget');
