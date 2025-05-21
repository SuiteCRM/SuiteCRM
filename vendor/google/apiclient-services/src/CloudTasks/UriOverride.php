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

namespace Google\Service\CloudTasks;

class UriOverride extends \Google\Model
{
  /**
   * @var string
   */
  public $host;
  protected $pathOverrideType = PathOverride::class;
  protected $pathOverrideDataType = '';
  /**
   * @var string
   */
  public $port;
  protected $queryOverrideType = QueryOverride::class;
  protected $queryOverrideDataType = '';
  /**
   * @var string
   */
  public $scheme;
  /**
   * @var string
   */
  public $uriOverrideEnforceMode;

  /**
   * @param string
   */
  public function setHost($host)
  {
    $this->host = $host;
  }
  /**
   * @return string
   */
  public function getHost()
  {
    return $this->host;
  }
  /**
   * @param PathOverride
   */
  public function setPathOverride(PathOverride $pathOverride)
  {
    $this->pathOverride = $pathOverride;
  }
  /**
   * @return PathOverride
   */
  public function getPathOverride()
  {
    return $this->pathOverride;
  }
  /**
   * @param string
   */
  public function setPort($port)
  {
    $this->port = $port;
  }
  /**
   * @return string
   */
  public function getPort()
  {
    return $this->port;
  }
  /**
   * @param QueryOverride
   */
  public function setQueryOverride(QueryOverride $queryOverride)
  {
    $this->queryOverride = $queryOverride;
  }
  /**
   * @return QueryOverride
   */
  public function getQueryOverride()
  {
    return $this->queryOverride;
  }
  /**
   * @param string
   */
  public function setScheme($scheme)
  {
    $this->scheme = $scheme;
  }
  /**
   * @return string
   */
  public function getScheme()
  {
    return $this->scheme;
  }
  /**
   * @param string
   */
  public function setUriOverrideEnforceMode($uriOverrideEnforceMode)
  {
    $this->uriOverrideEnforceMode = $uriOverrideEnforceMode;
  }
  /**
   * @return string
   */
  public function getUriOverrideEnforceMode()
  {
    return $this->uriOverrideEnforceMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UriOverride::class, 'Google_Service_CloudTasks_UriOverride');
