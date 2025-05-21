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

namespace Google\Service\SQLAdmin;

class InstancesListServerCertificatesResponse extends \Google\Collection
{
  protected $collection_key = 'serverCerts';
  /**
   * @var string
   */
  public $activeVersion;
  protected $caCertsType = SslCert::class;
  protected $caCertsDataType = 'array';
  /**
   * @var string
   */
  public $kind;
  protected $serverCertsType = SslCert::class;
  protected $serverCertsDataType = 'array';

  /**
   * @param string
   */
  public function setActiveVersion($activeVersion)
  {
    $this->activeVersion = $activeVersion;
  }
  /**
   * @return string
   */
  public function getActiveVersion()
  {
    return $this->activeVersion;
  }
  /**
   * @param SslCert[]
   */
  public function setCaCerts($caCerts)
  {
    $this->caCerts = $caCerts;
  }
  /**
   * @return SslCert[]
   */
  public function getCaCerts()
  {
    return $this->caCerts;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param SslCert[]
   */
  public function setServerCerts($serverCerts)
  {
    $this->serverCerts = $serverCerts;
  }
  /**
   * @return SslCert[]
   */
  public function getServerCerts()
  {
    return $this->serverCerts;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InstancesListServerCertificatesResponse::class, 'Google_Service_SQLAdmin_InstancesListServerCertificatesResponse');
