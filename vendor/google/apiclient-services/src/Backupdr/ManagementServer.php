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

class ManagementServer extends \Google\Collection
{
  protected $collection_key = 'networks';
  /**
   * @var string[]
   */
  public $baProxyUri;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  protected $managementUriType = ManagementURI::class;
  protected $managementUriDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $networksType = NetworkConfig::class;
  protected $networksDataType = 'array';
  /**
   * @var string
   */
  public $oauth2ClientId;
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
  public $state;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $updateTime;
  protected $workforceIdentityBasedManagementUriType = WorkforceIdentityBasedManagementURI::class;
  protected $workforceIdentityBasedManagementUriDataType = '';
  protected $workforceIdentityBasedOauth2ClientIdType = WorkforceIdentityBasedOAuth2ClientID::class;
  protected $workforceIdentityBasedOauth2ClientIdDataType = '';

  /**
   * @param string[]
   */
  public function setBaProxyUri($baProxyUri)
  {
    $this->baProxyUri = $baProxyUri;
  }
  /**
   * @return string[]
   */
  public function getBaProxyUri()
  {
    return $this->baProxyUri;
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
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
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
   * @param ManagementURI
   */
  public function setManagementUri(ManagementURI $managementUri)
  {
    $this->managementUri = $managementUri;
  }
  /**
   * @return ManagementURI
   */
  public function getManagementUri()
  {
    return $this->managementUri;
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
   * @param NetworkConfig[]
   */
  public function setNetworks($networks)
  {
    $this->networks = $networks;
  }
  /**
   * @return NetworkConfig[]
   */
  public function getNetworks()
  {
    return $this->networks;
  }
  /**
   * @param string
   */
  public function setOauth2ClientId($oauth2ClientId)
  {
    $this->oauth2ClientId = $oauth2ClientId;
  }
  /**
   * @return string
   */
  public function getOauth2ClientId()
  {
    return $this->oauth2ClientId;
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
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
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
   * @param WorkforceIdentityBasedManagementURI
   */
  public function setWorkforceIdentityBasedManagementUri(WorkforceIdentityBasedManagementURI $workforceIdentityBasedManagementUri)
  {
    $this->workforceIdentityBasedManagementUri = $workforceIdentityBasedManagementUri;
  }
  /**
   * @return WorkforceIdentityBasedManagementURI
   */
  public function getWorkforceIdentityBasedManagementUri()
  {
    return $this->workforceIdentityBasedManagementUri;
  }
  /**
   * @param WorkforceIdentityBasedOAuth2ClientID
   */
  public function setWorkforceIdentityBasedOauth2ClientId(WorkforceIdentityBasedOAuth2ClientID $workforceIdentityBasedOauth2ClientId)
  {
    $this->workforceIdentityBasedOauth2ClientId = $workforceIdentityBasedOauth2ClientId;
  }
  /**
   * @return WorkforceIdentityBasedOAuth2ClientID
   */
  public function getWorkforceIdentityBasedOauth2ClientId()
  {
    return $this->workforceIdentityBasedOauth2ClientId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ManagementServer::class, 'Google_Service_Backupdr_ManagementServer');
