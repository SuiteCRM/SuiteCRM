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

namespace Google\Service\OracleDatabase;

class CloudAccountDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $accountCreationUri;
  /**
   * @var string
   */
  public $cloudAccount;
  /**
   * @var string
   */
  public $cloudAccountHomeRegion;
  /**
   * @var string
   */
  public $linkExistingAccountUri;

  /**
   * @param string
   */
  public function setAccountCreationUri($accountCreationUri)
  {
    $this->accountCreationUri = $accountCreationUri;
  }
  /**
   * @return string
   */
  public function getAccountCreationUri()
  {
    return $this->accountCreationUri;
  }
  /**
   * @param string
   */
  public function setCloudAccount($cloudAccount)
  {
    $this->cloudAccount = $cloudAccount;
  }
  /**
   * @return string
   */
  public function getCloudAccount()
  {
    return $this->cloudAccount;
  }
  /**
   * @param string
   */
  public function setCloudAccountHomeRegion($cloudAccountHomeRegion)
  {
    $this->cloudAccountHomeRegion = $cloudAccountHomeRegion;
  }
  /**
   * @return string
   */
  public function getCloudAccountHomeRegion()
  {
    return $this->cloudAccountHomeRegion;
  }
  /**
   * @param string
   */
  public function setLinkExistingAccountUri($linkExistingAccountUri)
  {
    $this->linkExistingAccountUri = $linkExistingAccountUri;
  }
  /**
   * @return string
   */
  public function getLinkExistingAccountUri()
  {
    return $this->linkExistingAccountUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAccountDetails::class, 'Google_Service_OracleDatabase_CloudAccountDetails');
