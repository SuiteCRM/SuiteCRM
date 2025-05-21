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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SharePointSourcesSharePointSource extends \Google\Model
{
  /**
   * @var string
   */
  public $clientId;
  protected $clientSecretType = GoogleCloudAiplatformV1ApiAuthApiKeyConfig::class;
  protected $clientSecretDataType = '';
  /**
   * @var string
   */
  public $driveId;
  /**
   * @var string
   */
  public $driveName;
  /**
   * @var string
   */
  public $fileId;
  /**
   * @var string
   */
  public $sharepointFolderId;
  /**
   * @var string
   */
  public $sharepointFolderPath;
  /**
   * @var string
   */
  public $sharepointSiteName;
  /**
   * @var string
   */
  public $tenantId;

  /**
   * @param string
   */
  public function setClientId($clientId)
  {
    $this->clientId = $clientId;
  }
  /**
   * @return string
   */
  public function getClientId()
  {
    return $this->clientId;
  }
  /**
   * @param GoogleCloudAiplatformV1ApiAuthApiKeyConfig
   */
  public function setClientSecret(GoogleCloudAiplatformV1ApiAuthApiKeyConfig $clientSecret)
  {
    $this->clientSecret = $clientSecret;
  }
  /**
   * @return GoogleCloudAiplatformV1ApiAuthApiKeyConfig
   */
  public function getClientSecret()
  {
    return $this->clientSecret;
  }
  /**
   * @param string
   */
  public function setDriveId($driveId)
  {
    $this->driveId = $driveId;
  }
  /**
   * @return string
   */
  public function getDriveId()
  {
    return $this->driveId;
  }
  /**
   * @param string
   */
  public function setDriveName($driveName)
  {
    $this->driveName = $driveName;
  }
  /**
   * @return string
   */
  public function getDriveName()
  {
    return $this->driveName;
  }
  /**
   * @param string
   */
  public function setFileId($fileId)
  {
    $this->fileId = $fileId;
  }
  /**
   * @return string
   */
  public function getFileId()
  {
    return $this->fileId;
  }
  /**
   * @param string
   */
  public function setSharepointFolderId($sharepointFolderId)
  {
    $this->sharepointFolderId = $sharepointFolderId;
  }
  /**
   * @return string
   */
  public function getSharepointFolderId()
  {
    return $this->sharepointFolderId;
  }
  /**
   * @param string
   */
  public function setSharepointFolderPath($sharepointFolderPath)
  {
    $this->sharepointFolderPath = $sharepointFolderPath;
  }
  /**
   * @return string
   */
  public function getSharepointFolderPath()
  {
    return $this->sharepointFolderPath;
  }
  /**
   * @param string
   */
  public function setSharepointSiteName($sharepointSiteName)
  {
    $this->sharepointSiteName = $sharepointSiteName;
  }
  /**
   * @return string
   */
  public function getSharepointSiteName()
  {
    return $this->sharepointSiteName;
  }
  /**
   * @param string
   */
  public function setTenantId($tenantId)
  {
    $this->tenantId = $tenantId;
  }
  /**
   * @return string
   */
  public function getTenantId()
  {
    return $this->tenantId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SharePointSourcesSharePointSource::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SharePointSourcesSharePointSource');
