<?php

namespace SuiteCRM;

/**
 * Class SugarURIFilter
 * @package SuiteCRM
 * URI filter for HTMLPurifier
 * Approves only resource URIs that are in the list of trusted domains
 * Until we have comprehensive CSRF protection, we need to sanitize URLs in emails, etc.
 * to avoid CSRF attacks.
 */
#[\AllowDynamicProperties]
class URIFilter extends \HTMLPurifier_URIFilter
{
    /** @var string $name */
    public $name = 'SugarURIFilter';

    /** @var array $allowed */
    protected $allowed = array();

    /**
     * @param \HTMLPurifier_Config $config
     * @return bool|void
     */
    public function prepare($config)
    {
        $configurator = new \Configurator();
        $sugar_config = $configurator->config;

        if (!empty($sugar_config['security_trusted_domains'])
            && is_array($sugar_config['security_trusted_domains'])
        ) {
            $this->allowed = $sugar_config['security_trusted_domains'];
        }
    }

    /**
     * @param \HTMLPurifier_URI $uri
     * @param \HTMLPurifier_Config $config
     * @param \HTMLPurifier_Context $context
     * @return bool
     */
    public function filter(&$uri, $config, $context)
    {
        // skip non-resource URIs
        if (!$context->get('EmbeddedURI', true)) {
            return true;
        }

        if (!empty($uri->scheme)
            && strtolower($uri->scheme) != 'http'
            && strtolower($uri->scheme) != 'https'
        ) {
            // do not touch non-HTTP URLs
            return true;
        }

        // relative URLs permitted since email templates use it
        // if(empty($uri->host)) return false;
        // allow URLs with no query
        if (empty($uri->query)) {
            return true;
        }

        // allow URLs for known good hosts
        foreach ($this->allowed as $allow) {
            // must be equal to our domain or subdomain of our domain
            if ($uri->host == $allow
                || substr((string) $uri->host, -(strlen((string) $allow) + 1)) == ".$allow"
            ) {
                return true;
            }
        }

        // Here we try to block URLs that may be used for nasty XSRF stuff by
        // referring back to Sugar URLs
        // allow URLs that don't start with /? or /index.php?
        if (!empty($uri->path)
            && $uri->path != '/'
        ) {
            $lpath = strtolower($uri->path);
            if (substr($lpath, -10) != '/index.php'
                && $lpath != 'index.php'
            ) {
                return true;
            }
        }

        $query_items = array();
        parse_str(from_html($uri->query), $query_items);
        // weird query, probably harmless
        if (empty($query_items)) {
            return true;
        }
        // suspiciously like SugarCRM query, reject
        if (!empty($query_items['module'])
            && !empty($query_items['action'])
        ) {
            return false;
        }

        // looks like non-download entry point - allow only specific entry points
        if (!empty($query_items['entryPoint'])
            && !in_array(
                $query_items['entryPoint'],
                array(
                    'download',
                    'image',
                    'getImage'
                )
            )
        ) {
            return false;
        }

        return true;
    }
}
