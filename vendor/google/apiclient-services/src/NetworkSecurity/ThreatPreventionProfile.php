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

namespace Google\Service\NetworkSecurity;

class ThreatPreventionProfile extends \Google\Collection
{
  protected $collection_key = 'threatOverrides';
  protected $antivirusOverridesType = AntivirusOverride::class;
  protected $antivirusOverridesDataType = 'array';
  protected $severityOverridesType = SeverityOverride::class;
  protected $severityOverridesDataType = 'array';
  protected $threatOverridesType = ThreatOverride::class;
  protected $threatOverridesDataType = 'array';

  /**
   * @param AntivirusOverride[]
   */
  public function setAntivirusOverrides($antivirusOverrides)
  {
    $this->antivirusOverrides = $antivirusOverrides;
  }
  /**
   * @return AntivirusOverride[]
   */
  public function getAntivirusOverrides()
  {
    return $this->antivirusOverrides;
  }
  /**
   * @param SeverityOverride[]
   */
  public function setSeverityOverrides($severityOverrides)
  {
    $this->severityOverrides = $severityOverrides;
  }
  /**
   * @return SeverityOverride[]
   */
  public function getSeverityOverrides()
  {
    return $this->severityOverrides;
  }
  /**
   * @param ThreatOverride[]
   */
  public function setThreatOverrides($threatOverrides)
  {
    $this->threatOverrides = $threatOverrides;
  }
  /**
   * @return ThreatOverride[]
   */
  public function getThreatOverrides()
  {
    return $this->threatOverrides;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ThreatPreventionProfile::class, 'Google_Service_NetworkSecurity_ThreatPreventionProfile');
